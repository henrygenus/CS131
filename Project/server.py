# import asyncio
from time import time
# import aiohttp

# port 12000, 12001, 12002, 12003, 12004
# error checking not yet implemented


class Server:
    def __init__(self, name, port):
        self.m_name = name
        self.m_port = port
        self.m_filename = str(name) + "_log.txt"
        self.m_history = dict()
        self.m_connections = dict()

    def process_message(self, message):
        words = message.split()
        message_handler = {
            "WHATSAT": (lambda msg: self.query_handler(msg)),
            "IAMAT": (lambda msg: self.assertion_handler(msg)),
            "AT": (lambda msg: self.report_handler(msg))
        }
        response: Report = message_handler[words[0]](words[1:])
        return self.log(response)

    def report_handler(self, message):
        # send msg to communicable servers besides sender (message[1])
        return Report(message)

    def connect_server(self, servers):
        for name, port_number in servers:
            self.m_connections[name] = port_number

    def assertion_handler(self, message):
        client_name, long_lat, send_time = message
        msg = Report([self.m_name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        # send msg to communicable servers
        return msg

    def query_handler(self, message):
        client_name, radius, result_count = message
        msg = self.m_history[client_name]
        # ask google for location
        # send message and JSON back to client
        return msg

    # logs a Report object to logfile and history
    def log(self, msg):
        self.m_history[msg.m_client_name] = msg
        with open(self.m_filename, 'w+') as logfile:
            logfile.write(msg())

    def run(self):
        # loop with async I/O
        # (async) await read (message)
        # msg = processMessage(message)
        pass

    def dump(self):
        for message in self.m_history.values():
            print(message())
        for server in self.m_connections.keys():
            print("Connected to:" + server)


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


def main():
    srv = Server("test", 69)
    srv.process_message("IAMAT "
                        "kiwi.cs.ucla.edu "
                        "0 "
                        "0")
    srv.process_message("AT Hill "
                        "+0.263873386 "
                        "kiwi.cs.ucla.edu "
                        "+34.068930-118.445127 "
                        "1520023934.918963997")
    srv.process_message("WHATSAT kiwi.cs.ucla.edu 10 5")
    srv.dump()


if __name__ == "__main__":
    main()
