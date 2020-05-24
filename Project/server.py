# import asyncio
from time import clock_gettime, CLOCK_MONOTONIC
# import aiohttp

# port 12000, 12001, 12002, 12003, 12004
# error checking not yet implemented


class Server:
    def __init__(self, name, port):
        self.m_name = name
        self.m_port = port
        self.m_filename = str(name) + ".txt"
        self.m_history = dict()

    def process_message(self, message):
        words = message.split()
        message_handler = {
            "WHATSAT": (lambda msg: self.query_handler(msg)),
            "IMAT": (lambda msg: self.assertion_handler(msg)),
            "AT": (lambda msg: self.message_handler(msg))
        }
        response = message_handler[words[0]](words[1:])
        self.log(response)

    def query_handler(self, message):
        client_name = message[0]
        m_radius = message[1]
        m_result_count = message[2]
        pass

    def assertion_handler(self, message):
        client_name, long_lat, send_time = message
        msg = Report("AT"
                      + self.m_name
                      + str(clock_gettime(CLOCK_MONOTONIC) - send_time)
                      + client_name
                      + long_lat
                      + send_time)
        self.log(msg)
        # send msg to communicable servers

    def report_handler(self, message):
        self.log(Report(message))
        # send msg to communicable servers besides sender (message[1])

    def log(self, msg):
        self.m_history[msg.m_client_name] = msg
        # write msg() to log file

    def run(self):
        # loop with asynch I/O
        # asynch read (message)
        # log message
        # msg = processMessage(message)
        # m_history[msg.clientName()] = msg
        pass


class Report:
    def __init__(self, message):
        self.m_server_name = message[1]
        self.m_time_dif = message[2]
        self.m_client_name = message[3]
        self.m_long_lat = message[4]
        self.m_send_time = message[5]

    def __call__(self):
        return "AT" \
               + self.m_server_name \
               + self.m_time_dif \
               + self.m_client_name \
               + self.m_long_lat \
               + self.m_send_time

    def get_client_name(self):
        return self.m_client_name


def main():
    srv = Server("Henry", 69)


if __name__ == "__main__":
    main()
