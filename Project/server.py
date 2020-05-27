import asyncio
import aiohttp
import argparse
import constants
from re import sub
from json import dumps
from time import time


class Server:
    def __init__(self, name, port, ip='127.0.0.1', servers=list()):
        self.name = name
        self.ip = ip
        self.port = port
        self.history = dict()
        self.server = None
        self.message_handler = {
                "WHATSAT": (lambda m, w: self.query_handler(m, w)),
                "IAMAT": (lambda m, w: self.assertion_handler(m, w)),
                "AT": (lambda m, w: self.report_handler(m, w))
        }
        self.connections = dict()
        for name, port_number in servers:
            self.connections[port_number] = name

    def start(self):
        try:
            asyncio.run(self.run())
        except KeyboardInterrupt:
            self._log(f"-- Closing server '{self.name}'.")
            self.server.close()

    async def run(self):
        self.server = await asyncio.start_server(self.connect, self.ip, self.port)
        self._log('++ {} serving on {}'.format(self.name, self.server.sockets[0].getsockname()))
        async with self.server:
            await self.server.serve_forever()

    async def _respond(self, writer, msg):
        self._log(">> {} responded '{}'".format(self.name, str(msg)))
        writer.write(str(msg).encode())
        await writer.drain()

    @staticmethod
    def _log(msg):
        with open("log.txt", 'a+') as logfile:
            logfile.write(msg + "\n")
        if constants.DEBUG:
            print(msg)

    async def connect(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        await self.handle_message(await self.receive_message(reader), writer)

    async def receive_message(self, reader):
        msg = (await reader.read(constants.MAX_LEN)).decode()
        self._log("<< {} received '{}'".format(self.name, msg))
        return msg

    async def handle_message(self, message, writer):
        flood_statement = None
        try:
            words = message.split()
            flood_statement = await self.message_handler[words[0]](words[1:], writer)
        except (KeyError, IndexError):
            await self._respond(writer, f"? {message}")
        finally:
            writer.close()
            await writer.wait_closed()
            try:
                assert flood_statement is None
            except AssertionError:
                await self._flood(flood_statement)

    async def assertion_handler(self, msg_words, writer):
        client_name, long_lat, send_time = msg_words
        msg = Report([self.name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        self.history[msg.client_name] = msg
        await self._respond(writer, msg)
        return msg

    async def report_handler(self, msg_words, writer):
        msg = Report(msg_words)
        try:
            assert str(msg) == str(self.history[msg.client_name])
        except (KeyError, AssertionError):
            self.history[msg.client_name] = msg
            return msg
        finally:
            await self._respond(writer, msg)

    async def _flood(self, msg):
        for port, name in self.connections.items():
            try:
                (reader, writer) = await asyncio.open_connection(port=port)
                writer.write(str(msg).encode())
                await writer.drain()
                writer.close()
                await writer.wait_closed()
                self._log("|+ {} connected to {}".format(self.name, name))
                self._log(">> {} forwarded {}'".format(self.name, str(msg)))
            except IOError:
                self._log("|- {} failed to connect to {}".format(self.name, name))

    async def query_handler(self, msg_words, writer):
        client_name, radius, result_count = msg_words
        msg = self.history[client_name]
        jsons = await self.location_search(msg.lat_long, radius, result_count)
        await self._respond(writer, '{}\n{}'.format(str(msg), jsons))

    @staticmethod
    async def location_search(location, radius, result_count):
        results = await Server.make_request(location, radius, constants.APIKey)
        assert results['status'] == 'OK'
        del results['results'][int(result_count):]
        jsons = dumps(results, indent=4)
        return jsons

    @staticmethod
    async def make_request(loc, rad, key):
        lat_long = sub('([+-][0-9]+.[0-9]+)([+-][0-9]+.[0-9]+)', r'\1,\2', loc)
        async with aiohttp.ClientSession() as session:
            async with session.get("{}?key={}&location={}&radius={}".format(
                    constants.URL, key, lat_long, int(rad)*1000)) as response:
                return await response.json()


# FORMAT: SERVER_NAME DTIME CLIENT_NAME LONG_LAT SEND_TIME
class Report:
    def __init__(self, message):
        self.server_name,\
            self.time_dif, \
            self.client_name, \
            self.lat_long, \
            self.send_time = message

    def __str__(self):
        return "AT {} {} {} {} {}".format(
               self.server_name,
               self.time_dif,
               self.client_name,
               self.lat_long,
               self.send_time)


def make_server(name):
    if constants.SCOPE == "LOCAL":
        print("Using local ports")
        ports = constants.local_server_port_number
    else:
        print("Using remote ports")
        ports = constants.remote_server_port_number
    connected = list()
    for server_name in constants.connections[name]:
        connected.append((server_name, ports[server_name]))
    Server(name, ports[name], servers=connected).start()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()
    print(f"Hello, welcome to server {args.server_name}")
    make_server(args.server_name)


if __name__ == "__main__":
    main()
