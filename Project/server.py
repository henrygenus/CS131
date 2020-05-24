import asynchio
import aiohttp
import json
import time

# port 12000, 12001, 12002, 12003, 12004


class Server:
    def __init__(self, name, port):
        self.m_name = name
        self.m_port = port

    