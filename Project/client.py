import argparse
"""
The code comes from
https://asyncio.readthedocs.io/en/latest/tcp_echo.html

To run:
1. start a server first in a terminal
2. start the client.py in another terminal
"""

import asyncio


class Client:
    def __init__(self, port=8000, ip='127.0.0.1', name='client', message_max_length=1e6):
        """
        127.0.0.1 is the localhost
        port could be any port
        """
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        """
        on client side send the message for echo
        """
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print(f'{self.name} send: {message!r}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} received: {data.decode()!r}')

        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                asyncio.run(self.tcp_echo_client(message))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('server_port', type=int,
                        help='required server port input')
    args = parser.parse_args()
    client = Client(args.server_port)
    client.run_until_quit()
