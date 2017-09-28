import os
import json

# Everything we need will be available as env variables
TESTING = os.getenv('DEPENDENCIES_ENV') == 'test'

# All settings will be passed as strings. More complex types will be json encoded.
# All settings should be *optional*, so you must provide defaults.
SETTING_EXAMPLE_LIST = json.loads(os.getenv('SETTING_EXAMPLE_LIST', '[]'))
SETTING_EXAMPLE_STRING = os.getenv('SETTING_EXAMPLE_STRING', 'default')

# and we're currently working in the checked out git repo directory (/repo)
# in case we need to do anything with the actual repo files

print(f'List setting values: {SETTING_EXAMPLE_LIST}')
print(f'String setting value: {SETTING_EXAMPLE_STRING}')

# Do whatever we need to collect the dependencies, then just output the schema
collected_dependencies = [
    {
        'name': 'requests',
        'installed': {'version': '2.11.1'},
        'available': [{'version': '2.14.0'}],
        'path': 'requirements-to-freeze.txt',
        'source': 'pypi',
    },
    {
        'name': 'tox',
        'installed': {'version': '2.6.0'},
        'available': [{'version': '2.7.0'}],
        'path': 'requirements-to-freeze.txt',
        'source': 'pypi',
    }
]

schema_output = json.dumps({'dependencies': collected_dependencies})
print(f'BEGIN_DEPENDENCIES_SCHEMA_OUTPUT>{schema_output}<END_DEPENDENCIES_SCHEMA_OUTPUT')
