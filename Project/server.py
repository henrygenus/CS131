import asyncio
import argparse
from time import time
import aiohttp


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
        self.server_streams = list()
        self.server = None
        self.message_handler = {
                "WHATSAT": (lambda m, w: self.query_handler(m, w)),
                "IAMAT": (lambda m, w: self.assertion_handler(m, w)),
                "AT": (lambda m, w: self.report_handler(m, w))
        }
        for name, port_number in servers:
            self.connections[port_number] = name

    def start(self):
        try:
            self.log(f"Booting server '{self.name}'.")
            asyncio.run(self.run())
        except KeyboardInterrupt:
            self.log(f"--Closing server '{self.name}'.")
            self.server.close()

    def record(self, msg):
        self.history[msg.m_client_name] = msg
        self.log(f"-> {msg()}")

    def log(self, msg):
        with open(self.name + "_log.txt", 'a+') as logfile:
            logfile.write(msg + "\n")
        print(msg)

    async def run(self):
        self.server = await asyncio.start_server(self.connect, self.ip, self.port)
        await self.connect_servers()
        print(f'serving on {self.server.sockets[0].getsockname()}')
        async with self.server:
            await self.server.serve_forever()

    # TODO: log servers that connect to this one
    async def connect(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        print("Connected")
        try:
            msg = await reader.read(1024)
            message = msg.decode()
            addr = writer.get_extra_info('peername')
            print("{} received '{}' from {}".format(self.name, message, addr))
            await self.handle_message(message, writer)
        except NewConnection as e:
            self.server_streams.append(e.writer)
            self.log(f"Server '{e.name}' connected.")

    # TODO: make it so connection happens more than once
    async def connect_servers(self):
        for port, name in self.connections.items():
            try:
                (reader, writer) = await asyncio.open_connection(port=port)
                writer.write(f"AT CONNECT {self.name}".encode())
                self.server_streams.append(writer)
                self.log(f"Server '{name}' connected.")
            except IOError:
                print(f"Failed to connect to '{name}'.")

    async def handle_message(self, message, writer):
        try:
            self.log(f"{message} <-")
            words = message.split()
            msg: Report = await self.message_handler[words[0]](words[1:], writer)
            self.record(msg)
        except KeyError or IndexError:
            response = f"? {message}"
            writer.write(response.encode())
            self.log(f"-> {response}")

    async def assertion_handler(self, msg_words, writer):
        client_name, long_lat, send_time = msg_words
        msg = Report([self.name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        await self.flood(msg)
        writer.write(msg().encode())
        return msg

    async def flood(self, msg):
        print(self.server_streams)
        for writer in self.server_streams:
            try:
                await msg.send(writer)
                self.log(f"-> {msg()}")
            except IOError:
                pass

    async def report_handler(self, msg_words, writer):
        try:
            msg = Report(msg_words)
            assert msg == self.history[msg.get_client_name]
            await self.flood(msg)
            return msg
        except AssertionError:
            return msg
        except ValueError:
            if msg_words[0] == "CONNECT":
                raise NewConnection(name=msg_words[1], writer=writer)

    async def query_handler(self, msg_words, writer):
        client_name, radius, result_count = msg_words
        msg = self.history[client_name]
        await self.location_search(msg.get_location, radius, result_count)
        # record/send message and JSON back to client
        writer.write(msg().encode())
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
