import threading
import asyncio
from os import system
import server
import echo_client
import sys

remote_server_port_number = {
     'Hill': 12000,
     'Jaquez': 12001,
     'Smith': 12002,
     'Campbell': 12003,
     'Singleton': 12004
}
local_server_port_number = {
     'Hill': 8000,
     'Jaquez': 8001,
     'Smith': 8002,
     'Campbell': 8003,
     'Singleton': 8004
}
connections = {
        "Hill": ["Jaquez", "Smith"],
        "Singleton": ["Jaquez", "Smith", "Campbell"],
        "Smith": ["Campbell", "Singleton"],
        "Jaquez": ["Singleton", "Hill"],
        "Campbell": ["Singleton", "Smith"]
}


async def make_server(name):
    if sys.argv[1] == "local":
        ports = local_server_port_number
    else:
        ports = remote_server_port_number
    connected = list()
    for server_name in connections[name]:
        connected.append((server_name, ports[server_name]))
    server.Server(name, ports[name], servers=connected).start()


def main():
    asyncio.create_task(make_server("Hill"))
    asyncio.create_task(make_server("Singleton"))
    asyncio.create_task(make_server("Jaquez"))
    asyncio.create_task(make_server("Smith"))
    asyncio.create_task(make_server("Campbell"))

    client = echo_client.Client()
    client.run_until_quit()



if "__name__" == "__main__":
    main()