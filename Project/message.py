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
