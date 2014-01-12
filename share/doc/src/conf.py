## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

import datetime
import os
import json
import re
import subprocess
import sys

sys.path.insert(0, os.path.abspath('../ext'))

extensions = ["sphinx.ext.todo", "sphinx.ext.extlinks", 'github',
              'httpdomain', 'configdomain']


# read packagesinfos
_source = "../../../pkg.vars.config"
_cmd = "../../support/doc/config_to_json.escript"
_output = subprocess.check_output([_cmd, _source])
_info = json.loads(_output)


source_suffix = ".rst"

nitpicky = True

version = "%s.%s" % (
        _info['version_major'],
        _info['version_minor']
)

release = "%s.%s.%s" % (
        _info['version_major'],
        _info['version_minor'],
        _info['version_revision']
)

print(release)
if _info.get('version_release') == '.%revision%':
    release += '-dev'
elif _info.get('version_release'):
    # jenkins hack, the release name is too long or uses
    # characters that cause pain down the road. Example:
    # 1.6.0+build.jenkins-ERLANG_VERSION=R14B04,label=Mac-OS-10-8-2-832-76-g2996574
    # which breaks the LaTeX PDF build. Let’s strip this
    # down to the git hash at the end.
    if 'jenkins' in _info['version_release']:
        release += _info['version_release'][-9:]
    else: # regular case
        release += _info['version_stage'] + _info['version_release']


project = _info['vendor_name']

copyright = '%d, %s' % (
    datetime.datetime.now().year,
    _info['vendor_contact_name']
)

highlight_language = "json"

primary_domain = "http"

pygments_style = "sphinx"

html_theme = "couchdb"

html_theme_path = ['../templates']

templates_path = ["../templates"]

html_static_path = ["../static"]

html_title = ' '.join([
    project,
    version,
    'Documentation'
])

html_style = "rtd.css"

html_logo = "../images/logo.png"

html_favicon = "../images/favicon.ico"

html_use_index = False

html_additional_pages = {
    'download': 'pages/download.html',
    'index': 'pages/index.html'
}

html_context = {}

html_sidebars = {
    "**": [
        "searchbox.html",
        "localtoc.html",
        "relations.html",
        "utilities.html",
        "help.html",
    ]
}

text_newlines = "native"

latex_documents = [(
    "contents",
    "CouchDB.tex",
    project,
    "",
    "manual",
    True
)]

latex_elements = {
    "papersize": "a4paper"
}

texinfo_documents = [(
    "contents",
    "CouchDB",
    project,
    "",
    "CouchDB",
    "The Apache CouchDB database",
    "Databases",
    True
)]

extlinks = {
    'issue': ('%s-%%s' % _info['package_bugreport'], 'COUCHDB-'),
    'commit': ('https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=commit;h=%s', '#')
}

github_project = 'apache/couchdb'

html_context['git_branch'] = github_branch = 'master'

github_docs_path = 'share/doc/src'

del _info, _source, _cmd, _output
