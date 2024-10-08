# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html
from __future__ import annotations

import os
import sys
from datetime import datetime

sys.path.insert(0, os.path.abspath("../src"))


# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = "gamstransfer"
copyright = f"{datetime.now().year}, GAMS Development Corporation"
author = "GAMS"
# release = "3.0.2"
language = "en"
html_last_updated_fmt = ""  # to reveal the build date in the pages meta

# -- Switcher ----------------------------------------------------------------
# Define the json_url for our version switcher.
# json_url = "https://gamstransfer.readthedocs.io/en/latest/_static/switcher.json"

# is_readthedocs = os.environ.get("READTHEDOCS_VERSION", "dev")
# if is_readthedocs == "dev":
#     json_url = "docs/_static/switcher.json"

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.doctest",
    "sphinx.ext.duration",
    "sphinx.ext.intersphinx",
    "sphinx.ext.mathjax",
    "sphinx.ext.todo",
    "sphinx.ext.viewcode",
    "sphinx.ext.coverage",
    "sphinx.ext.graphviz",
    "sphinx.ext.ifconfig",
    "sphinx_design",
    "sphinx_copybutton",
    "nbsphinx",
    "numpydoc",
    "matplotlib.sphinxext.plot_directive",
    "IPython.sphinxext.ipython_console_highlighting",
    "IPython.sphinxext.ipython_directive",
    "sphinx.ext.mathjax",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx.ext.napoleon",
]

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = "pydata_sphinx_theme"
html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_show_sourcelink = False
html_theme_options = {
    "icon_links": [
        {
            "name": "CRAN",
            "url": "https://cran.r-project.org/web/packages/gamstransfer/index.html",
            "icon": "_static/Rlogo.png",
            "type": "local",
            "attributes": {"target": "_blank"},
        },
        {
            "name": "GitHub",
            "url": "https://github.com/GAMS-dev/transfer-r",
            "icon": "fa-brands fa-square-github",
            "type": "fontawesome",
        },
        {
            "name": "Twitter",
            "url": "https://twitter.com/GamsSoftware",
            "icon": "fa-brands fa-square-twitter",
        },
        {
            "name": "GAMS",
            "url": "https://www.gams.com",
            "icon": "_static/gams.svg",
            "type": "local",
            "attributes": {"target": "_blank"},
        },
    ],
    "logo": {
        "image_light": "_static/logo.png",
        "image_dark": "_static/logo.png",
    },
    # "switcher": {
    #     "json_url": json_url,
    #     "version_match": f"v{release}",
    # },
    # "navbar_center": ["version-switcher", "navbar-nav"],
    "show_nav_level": 2,
    "show_toc_level": 2,
}

autodoc_default_options = {'member-order': 'bysource'}

autodoc_member_order = "bysource"

# Display todos by setting to True
todo_include_todos = False

# temporary flag until the next release
nbsphinx_allow_errors = True