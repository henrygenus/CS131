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
        self.connections = dict()
        self.connected = list()
        self.server = None
        self.message_handler = {
                "WHATSAT": (lambda m: self.query_handler(m)),
                "IAMAT": (lambda m: self.assertion_handler(m)),
                "AT": (lambda m: self.report_handler(m))
        }
        for name, port_number in servers:
            self.connections[port_number] = (False, name)

    def start(self):
        try:
            asyncio.run(self.run())
        except KeyboardInterrupt:
            self.log(f"-- Closing server '{self.name}'.")
            self.server.close()

    @staticmethod
    def log(msg):
        with open("log.txt", 'a+') as logfile:
            logfile.write(msg + "\n")
        print(msg)

    async def run(self):
        self.server = await asyncio.start_server(self.connect, self.ip, self.port)
        self.log('++ {} serving on {}'.format(self.name, self.server.sockets[0].getsockname()))
        async with self.server:
            await self.server.serve_forever()

    async def connect(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        msg = await reader.read(constants.MAX_LEN)
        message = msg.decode()
        self.log("<< {} received '{}'".format(self.name, message))
        await self.handle_message(message, writer)

    async def handle_message(self, message, writer):
        response = ""
        try:
            words = message.split()
            response = await self.message_handler[words[0]](words[1:])
        except KeyError or IndexError:
            response = f"? {message}"
        finally:
            self.log(">> {} responded '{}'".format(self.name, response))
            writer.write(response.encode())
            await writer.drain()
            writer.close()

    async def assertion_handler(self, msg_words):
        client_name, long_lat, send_time = msg_words
        msg = Report([self.name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        self.history[msg.m_client_name] = msg
        await self.flood(msg)
        return msg()

    async def report_handler(self, msg_words):
        msg = Report(msg_words)
        try:
            assert msg() == self.history[msg.get_client_name()]()
        except KeyError or AssertionError:
            self.history[msg.get_client_name()] = msg
            await self.flood(msg)
        finally:
            return msg()

    # TODO: don't reply to sender since they have a writer open
    """
    ERRORS ON:
    dropped Jaquez, Singleton
    dropped Hill, Smith
    dropped Hill, Campbell
    ie when there is a looping recursion
    """
    async def flood(self, msg):
        for port, (was_open, name) in self.connections.items():
            try:
                print(f"{self.name} trying to flood {name}")
                (reader, writer) = await asyncio.open_connection(port=port)
                await msg.send(writer)
                writer.close()

                self.connections[port] = (True, name)
                try:
                    assert was_open
                except AssertionError:
                    self.log("|+ {} connected to {}".format(self.name, name))
                finally:
                    self.log(">> {} forwarded '{}'".format(self.name, msg()))

            except IOError:
                self.connections[port] = (False, name)
                try:
                    assert not was_open
                except AssertionError:
                    self.log("|- {} disconnected from {}".format(self.name, name))

    async def query_handler(self, msg_words):
        client_name, radius, result_count = msg_words
        msg = self.history[client_name]
        jsons = await self.location_search(msg.get_location(), radius, result_count)
        return '{}{}{}'.format(msg(), "\n", jsons)

    @staticmethod
    async def location_search(location, radius, result_count):
        results = await Server.make_request(location, radius, constants.APIKey)
        del results['results'][int(result_count):]
        jsons = dumps(results, indent=4)
        return jsons

    @staticmethod
    async def make_request(loc, rad, key):
        lat_long = sub('([+-][0-9]+.[0-9]+)([+-][0-9]+.[0-9]+)', r'\1,\2', loc)
        async with aiohttp.ClientSession() as session:
            async with session.get("{}?key={}&location={}&radius={}".format(
                    constants.URL, key, lat_long, int(rad)*1000)) as response:
                print(response.url)
                return await response.json()


# FORMAT: SERVER_NAME DTIME CLIENT_NAME LONG_LAT SEND_TIME
class Report:
    def __init__(self, message):
        self.m_server_name,\
            self.m_time_dif, \
            self.m_client_name, \
            self.m_long_lat, \
            self.m_send_time = message

    def __call__(self):
        return "AT {} {} {} {} {}".format(
               self.m_server_name,
               self.m_time_dif,
               self.m_client_name,
               self.m_long_lat,
               self.m_send_time)

    def get_client_name(self):
        return self.m_client_name

    def get_location(self):
        return self.m_long_lat

    async def send(self, writer):
        try:
            writer.write(self().encode())
            await writer.drain()
            writer.close()
        except IOError:
            pass


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
