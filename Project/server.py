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

    def get_name(self):
        return self.m_name

    def connect_server(self, servers):
        for name, port_number in servers:
            self.m_connections[name] = port_number

    def process_message(self, message):
        words = message.split()
        message_handler = {
            "WHATSAT": (lambda msg: self.query_handler(msg)),
            "IAMAT": (lambda msg: self.assertion_handler(msg)),
            "AT": (lambda msg: self.report_handler(msg))
        }
        response: Report = message_handler[words[0]](words[1:])
        return self.log(response)

    def report_handler(self, msg_words):
        msg = Report(msg_words)
        self.flood(msg)
        return msg

    def assertion_handler(self, message):
        client_name, long_lat, send_time = message
        msg = Report([self.m_name,
                      str(time() - float(send_time)),
                      client_name,
                      long_lat,
                      send_time])
        if msg == self.m_history[msg.get_client_name]:
            pass
        else:
            self.flood(msg)
        return msg

    def query_handler(self, message):
        client_name, radius, result_count = message
        msg = self.m_history[client_name]
        self.location_search(msg.get_location, radius, result_count)
        # record/send message and JSON back to client
        return msg

    def location_search(self, location, radius, result_count):
        # query google for nearby
        pass

    def run(self):
        while True:  # loop with async I/O
            # (async) await read (message)
            message = ""  # null declaration to run
            self.process_message(message)

    def record(self, msg):
        self.m_history[msg.m_client_name] = msg
        self.log(msg())

    def log(self, msg):
        with open(self.m_filename, 'w+') as logfile:
            logfile.write(msg)
        return msg

    def flood(self, msg):
        for server in self.m_connections.values():
            try:
                msg.send(server)
            except IOError:
                self.log("Server " + server.get_name() + " disconnected")
                pass
            pass

    def dump(self):
        for message in self.m_history.values():
            print(message())
        for server in self.m_connections.keys():
            print("Connected to:" + server)


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

    def send(self, recipient):
        try:
            int(recipient)
            # send self message to recipient port number
            pass
        except TypeError:
            # async error
            # send self message to recipient address
            pass


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
