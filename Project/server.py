import asyncio
import argparse
from time import time
import aiohttp


class Server:
    def __init__(self, name, port, ip='127.0.0.1', servers=list()):
        self.name = name
        self.ip = ip
        self.port = port
        self.history = dict()
        self.connections = dict()
        self.server_streams = dict()
        self.server = None
        for name, port_number in servers:
            self.connections[port_number] = name

    def start(self):
        try:
            asyncio.run(self.run())
        except KeyboardInterrupt:
            self.log("\nServer '" + self.name + "' closing.")
            self.server.close()

    async def run(self):
        self.server = await asyncio.start_server(self.connect, self.ip, self.port)
        print(f'serving on {self.server.sockets[0].getsockname()}')
        async with self.server:
            await self.server.serve_forever()

    async def connect(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        msg = await reader.read(1024)
        message = msg.decode()
        addr = writer.get_extra_info('peername')
        print("{} received '{}' from {}".format(self.name, message, addr))
        await self.handle_message(message, writer)
        print("Awaiting messages.")

    async def handle_message(self, message, writer):
        try:
            self.log(message + " <-")
            words = message.split()
            message_handler = {
                "WHATSAT": (lambda m: self.query_handler(m)),
                "IAMAT": (lambda m: self.assertion_handler(m)),
                "AT": (lambda m: self.report_handler(m))
            }
            msg: Report = message_handler[words[0]](words[1:])
            self.record(msg)
            response = msg()
        except KeyError:
            response = "? " + message
            self.log("-> " + response)
        finally:
            writer.write(response.encode())
            await writer.drain()

    async def connect_servers(self):
        for name, port in self.connections:
            try:
                (reader, writer) = await asyncio.open_connection(port=port)
                self.server_streams[name] = (reader, writer)
                await asyncio.create_task(self.connect(reader, writer))
            except IOError:
                pass

    def report_handler(self, msg_words):
        msg = Report(msg_words)
        self.flood(msg)
        return msg

    def assertion_handler(self, message):
        client_name, long_lat, send_time = message
        msg = Report([self.name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        try:
            assert msg == self.history[msg.get_client_name]
        except KeyError or AssertionError:
            self.flood(msg)
        return msg

    def query_handler(self, message):
        client_name, radius, result_count = message
        msg = self.history[client_name]
        self.location_search(msg.get_location, radius, result_count)
        # record/send message and JSON back to client
        return msg

    def location_search(self, location, radius, result_count):
        # query google for nearby
        pass

    def record(self, msg):
        self.history[msg.m_client_name] = msg
        self.log("->" + msg())

    def log(self, msg):
        with open(self.name + "_log.txt", 'w+') as logfile:
            logfile.write(msg)
        print(msg)

    def flood(self, msg):
        for reader, writer in self.server_streams.values():
            try:
                msg.send(writer)
            except IOError:
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
        return "AT " \
               + self.m_server_name + " "\
               + self.m_time_dif + " "\
               + self.m_client_name + " "\
               + self.m_long_lat + " "\
               + self.m_send_time

    def get_client_name(self):
        return self.m_client_name

    def get_location(self):
        return self.m_long_lat

    async def send(self, writer):
        try:
            writer.write(self())
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
    print("Hello, welcome to server {}".format(args.server_name))
    server = Server(args.server_name, args.server_port)
    server.start()


if __name__ == "__main__":
    main()
