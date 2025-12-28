VENV       := .venv
VENV_STAMP := .venv_done
PYTHON     := python3

MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.ONESHELL:

# List of goals not corresponding to file names.

.PHONY : all clean distclean venv test

# Default goal

all : main

# Rules for building

venv: $(VENV)/$(VENV_STAMP)

$(VENV)/$(VENV_STAMP): requirements.txt
    ifneq ($(wildcard $(VENV)/.),)
        $(error Error: The directory '$(VENV)' already exists. Please remove it before proceeding)
    endif
    $(PYTHON) -m venv $(VENV)
    source $(VENV)/bin/activate
    pip install -Ur requirements.txt
    $(file >$@)

demo : | venv
    $(PYTHON) driver/driver.py --demo

clean:
    $(RM) -r $(VENV)
