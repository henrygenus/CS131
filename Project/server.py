import asyncio
import argparse
from time import time
# import aiohttp


class NewConnection(RuntimeError):
    def __init__(self, name, writer):
        self.name = name
        self.writer = writer


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
        msg = await reader.read(1024)
        message = msg.decode()
        self.log("<< {} received '{}'".format(self.name, message))
        await self.handle_message(message, writer)

    async def handle_message(self, message, writer):
        response = f"? {message}"
        try:
            words = message.split()
            msg: Report = await self.message_handler[words[0]](words[1:])
            self.history[msg.m_client_name] = msg
            response = msg()
        except KeyError or IndexError:
            pass
        finally:
            self.log(">> {} responded '{}'".format(self.name, response))
            writer.write(response.encode())

    async def assertion_handler(self, msg_words):
        client_name, long_lat, send_time = msg_words
        msg = Report([self.name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        await self.flood(msg)
        return msg

    # TODO: make flood algorithm work
    async def report_handler(self, msg_words):
        msg = Report(msg_words)
        try:
            assert msg() == self.history[msg.get_client_name()]()
        except KeyError or AssertionError:
            self.history[msg.get_client_name()] = msg
            await self.flood(msg)
        finally:
            return msg

    async def flood(self, msg):
        for port, (was_open, name) in self.connections.items():
            try:
                (reader, writer) = await asyncio.open_connection(port=port)
                await msg.send(writer)
                writer.close()
                assert was_open
            except AssertionError:
                self.connections[port] = (True, name)
                self.log("|+ {} connected to {}".format(self.name, name))
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
        await self.location_search(msg.get_location, radius, result_count)
        # record/send message and JSON back to client
        return msg

    async def location_search(self, location, radius, result_count):
        # query google for nearby
        pass


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
        except IOError:
            pass


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    parser.add_argument('server_port', type=int,
                        help='required server port input')
    args = parser.parse_args()
    print(f"Hello, welcome to server {args.server_name}")
    server = Server(args.server_name, args.server_port)
    server.start()


if __name__ == "__main__":
    main()
