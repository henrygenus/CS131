URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
APIKey = "AIzaSyAeqJNnvBtHQ3L0G0uNjcVhzKqbIOEbAPs"
MAX_LEN = int(1e5)
connections = {
    "Hill": ["Jaquez", "Smith"],
    "Jaquez": ["Singleton", "Hill"],
    "Smith": ["Singleton", "Campbell", "Hill"],
    "Singleton": ["Jaquez", "Smith", "Campbell"],
    "Campbell": ["Smith", "Singleton"]
}

DEBUG = False

SCOPE = "LOCAL"
local_server_port_number = {
    'Hill': 8000,
    'Jaquez': 8001,
    'Smith': 8002,
    'Campbell': 8003,
    'Singleton': 8004
}
remote_server_port_number = {
    "Hill": 12000,
    "Jaquez": 12001,
    "Smith": 12002,
    "Campbell": 12003,
    "Singleton": 12004
}