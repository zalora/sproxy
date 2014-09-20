import sys
from wsgiref.simple_server import make_server

try:
    port = int(sys.argv[1])
except IndexError:
    port = 8080

def simple_app(_, start_response):
    start_response('200 OK', [('Content-type', 'text/plain')])
    return 'foo'

make_server('', int("4061"), simple_app).serve_forever()
