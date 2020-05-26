import server
import argparse

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


def make_server(name, scope):
    if scope == "local":
        print("Using local ports")
        ports = local_server_port_number
    else:
        print("Using remote ports")
        ports = remote_server_port_number
    connected = list()
    for server_name in connections[name]:
        connected.append((server_name, ports[server_name]))
    server.Server(name, ports[name], servers=connected).start()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    parser.add_argument('scope', type=str,
                        help='required) server scope input')
    args = parser.parse_args()
    print("Hello, welcome to server {}".format(args.server_name))
    make_server(args.server_name, args.scope)


if __name__ == "__main__":
    main()
