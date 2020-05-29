# Github Personality Inference
Personality inference application for the study "Inferring Personality from GitHub Communication Data: Promises &amp; Perils"

## Installation
### Requirements
For the Personality Inference application to work, serveral installation must
be installed beforehand.
- MySQL database
- Python >=3.7
- LIWC for running Yarkoni and Golbeck.
- Personality Insights access keys for running PI.

### Running
To actually install the application, one would need Python to connect to a
MySQL database. The host address and the name of the database to connect to
can all be configured in the `config.json` file.

Whenever the database uses a password to secure the connection to the database
(you are strongly encouraged to do so), the password must be set as an
Environment Variable.

By creating a file inside the repository with the name `keys.json` (see `example_keys.json`) one can enable multiple PI keys and see when the key has been fully used (if a limit is used). If there is a limit, the limit is reset at the first of each month by PI.

All operations run from the `main.py` file based on the configurations of the `config.json`.

### Packages
The repository is divided in three sections:
- The main folder, containing the Python files for the main program.
- The `stats` folder, containing all R files with the statistical analysis.
- The `tests` folder, containing some tests used in early development.
