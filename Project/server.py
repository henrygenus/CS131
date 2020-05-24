# import asynchio
# import aiohttp
import json
import time

# port 12000, 12001, 12002, 12003, 12004
# error checking not yet implemented


class Server:
    message_handler = {
        "WHATSAT": (lambda message: QueryMessage(message)),
        "IMAT": (lambda message: AssertionMessage(message)),
        "AT": (lambda message: ReportMessage(message))
    }
    m_history = dict()

    def __init__(self, name, port):
        self.m_name = name
        self.m_port = port
        self.m_filename = str(name) + ".txt"

    def run(self):
        # loop with asynch I/O
        # asynch read (message)
        # log message
        # msg = processMessage(message)
        # m_history[msg.clientName()] = msg
        pass

    @staticmethod
    def process_message(message):
        Server.message_handler[message[0]](message[1:])

# message interface:
#    __init__(message)
#    __call__()
#    get_client_name()
# anything else can be below the surface

# ######### MESSAGE CLASSES ######### #


class QueryMessage:
    def __init__(self, message):
        self.m_client_name = message[3]

    def __call__(self):
        pass

    def get_client_name(self):
        return self.m_client_name


class AssertionMessage:
    def __init__(self, message):
        self.m_client_name = message[3]

    def __call__(self):
        pass

    def get_client_name(self):
        return self.m_client_name


class ReportMessage:
    def __init__(self, message):
        self.m_client_name = message[3]

    def __call__(self):
        pass

    def get_client_name(self):
        return self.m_client_name


def main():
    srv = Server("Henry", 69)


if __name__ == "__main__":
    main()
