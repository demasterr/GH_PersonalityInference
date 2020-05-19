# Github Personality Inference
Personality inference application for the study "Inferring Personality from GitHub Communication Data: Promises &amp; Perils"

## Installation
### Requirements
For the Personality Inference application to work, serveral installation must
be installed beforehand.
- MySQL database
- Python >3.7

### Steps
To actually install the application, one would need Python to connect to a
MySQL database. The host address and the name of the database to connect to
can all be configured in the `config.json` file.

Whenever the database uses a password to secure the connection to the database
(you are strongly encouraged to do so), the password must be set as an
Environment Variable.
