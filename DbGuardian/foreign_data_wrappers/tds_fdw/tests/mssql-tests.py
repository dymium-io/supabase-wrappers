#!/usr/bin/env python
from lib.messages import print_error, print_info, print_ok, print_report
from lib.messages import print_usage_error, print_warning
from lib.tests import run_tests
from optparse import OptionParser
from os import path

DEFAULT_TDS_VERSION="7.1"

try:
    from pymssql import connect
except:
    print_error(
        "pymssql library not available, please install it before usage!")
    exit(1)


def parse_options():
    """Parse and validate options. Returns a dict with all the options."""
    usage = "%prog <arguments>"
    description = ('Run MSSQL tests from sql files')
    parser = OptionParser(usage=usage, description=description)
    parser.add_option('--server', action='store',
                      help='MSSQL/Azure server')
    parser.add_option('--port', action='store',
                      help='MSSQL/Azure  TCP port')
    parser.add_option('--database', action='store',
                      help='Database name')
    parser.add_option('--schema', action='store',
                      help='Schema to use (and create if needed)')
    parser.add_option('--username', action='store',
                      help='Username to connect')
    parser.add_option('--password', action='store',
                      help='Passord to connect')
    parser.add_option('--azure', action='store_true', default=False,
                      help='If present, will connect as Azure, otherwise as '
                           'standard MSSQL')
    parser.add_option('--tds_version', action="store", default=DEFAULT_TDS_VERSION,
                      help='Specifies th TDS protocol version, default="%s"'%DEFAULT_TDS_VERSION)
    (options, args) = parser.parse_args()
    # Check for test parameters
    if (options.server is None or
        options.port is None or
        options.database is None or
        options.schema is None or
        options.username is None or
            options.password is None):
        print_error("Insufficient parameters, check help (-h)")
        exit(4)
    else:
        if options.azure is True:
            options.username = '%s@%s' % (
                options.username, options.server.split('.')[0])
        return(options)


def main():
    try:
        args = parse_options()
    except Exception as e:
        print_usage_error(path.basename(__file__), e)
        exit(2)
    try:
        conn = connect(server=args.server, user=args.username,
                       password=args.password, database=args.database,
                       port=args.port, tds_version=args.tds_version)
        replaces = {'@SCHEMANAME': args.schema}
        tests = run_tests('tests/mssql/*.sql', conn, replaces, 'mssql')
        print_report(tests['total'], tests['ok'], tests['errors'])
        if tests['errors'] != 0:
            exit(5)
        else:
            exit(0)
    except Exception as e:
        print_error(e)
        exit(3)

if __name__ == "__main__":
    main()
