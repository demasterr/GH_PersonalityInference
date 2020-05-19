"""
Module containing all connection related classes.
"""
import json
import os
import sys
from datetime import date
from datetime import datetime
from time import sleep
from typing import List

import mysql.connector
from ibm_cloud_sdk_core import ApiException
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator
from ibm_watson import PersonalityInsightsV3
from mysql.connector import errorcode, MySQLConnection

from config import Config


class MySQLConnector:
    """
    Class for handling MySQL connections.
    """
    _CONNECTION: MySQLConnection = None

    @staticmethod
    def execute(query: str, has_return=True, limit=-1) -> List[tuple]:
        """
        Query a SQL statement and return the resulting tuple.

        This function only accepts single line query statements.
        :param query: SQL query in string format.
        :param has_return: Boolean indicating if a result is expected and should be fetched.
        :param limit: Limit on number of results. Only used when above 0.
        :return: Resulting tuple containing the queried rows or an empty list of nothing to return.
        """
        if limit > 0:
            query += " LIMIT {}".format(limit)
        # Execute the query.
        connection = MySQLConnector.get_connection()
        connection.autocommit = True
        cursor = connection.cursor()
        cursor.execute(query)
        if has_return:
            return cursor.fetchall()
        return cursor

    @staticmethod
    def execute_until(query: str, has_return=True, limit=-1) -> List[tuple]:
        """
        Query multiple SQL statements until rows result from a query.

        This function accepts multiple query statements but will only return the result of the first
        query to result in rows. This function should not be used for, e.g., multiple select
        queries.
        :param query: SQL queries in string format.
            The string consists of multiple SQL query statements.
        :param has_return: Boolean indicating if a result is expected and should be fetched.
        :param limit: Limit on number of results. Only used when above 0.
        :return: Resulting tuple containing the queried rows of the first row resulting SQL query.
        """
        if limit > 0:
            query += " LIMIT {}".format(limit)
        connection = MySQLConnector.get_connection()
        connection.autocommit = True
        results = connection.cursor().execute(query, multi=True)
        for result in results:
            if has_return and result.with_rows:
                return result.fetchall()
        return []

    @staticmethod
    def get_connection():
        """
        Get the connection for the database and create one if no connection exists yet.
        :return: Connection instance to MySQL database.
        """
        # Create a new connection of none exsists.
        if not MySQLConnector._CONNECTION:
            MySQLConnector._connect()
            return MySQLConnector._CONNECTION
        # Return an already existing connection.
        return MySQLConnector._CONNECTION

    @staticmethod
    def _connect():
        """
        Connect to the MySQL database.
        """
        config = Config.get_sql_config()
        if not config:
            print('Config could not be loaded properly.')
            sys.exit(0)
        try:
            MySQLConnector._CONNECTION = mysql.connector.connect(**config)
        except mysql.connector.Error as err:
            if err.errno == errorcode.ER_ACCESS_DENIED_ERROR:
                print("Something is wrong with your user name or password")
            elif err.errno == errorcode.ER_BAD_DB_ERROR:
                print("Database does not exist")
            elif err.errno == errorcode.CR_CONN_HOST_ERROR:
                print("Could not connect to database. Is it online?")
            else:
                print("Unknown error: " + str(err))
            print("The application cannot run without database access.")
            sys.exit()

    @staticmethod
    def close():
        """
        Close the current connection if one exists.
        """
        if MySQLConnector._CONNECTION:
            MySQLConnector._CONNECTION.close()

    @staticmethod
    def insert(table, values, cols):
        """
        Insert a row in the database.
        """
        MySQLConnector.execute("INSERT INTO {}{} VALUES {}".format(table, values, cols),
                               has_return=False)

    @staticmethod
    def commit():
        """
        Because it is in most cases faster to commit multiple insertions, this function can be used
        to finally commit after multiple inserts have been sent to the database.
        """
        MySQLConnector.get_connection().commit()


class IBMConnector:
    """
    Class used for connectoins to the IBM Watson Personality Insights service.
    Personality Insights may also be referred to as PI.
    """

    def __init__(self, version='2017-10-13'):
        """
        Initialize a connection to the IBM Watson Personality Insights service.
        :param version: Version used for the PI
        """
        self.version = version
        self.current_key = 0
        key, url = self.get_key()
        self._initialize(key, url)

    def _initialize(self, key, url):
        """
        Initialize the IBM Connector with the key and url specified.
        :param key: API Key for the PI service.
        :param url: URL to the API service.
        """
        self.authenticator = IAMAuthenticator(key)
        self.personality_insights = PersonalityInsightsV3(
            version=self.version,
            authenticator=self.authenticator
        )
        self.personality_insights.set_default_headers({'x-watson-learning-opt-out': "true"})
        self.personality_insights.set_service_url(url)

    @staticmethod
    def _read_keys():
        """
        Get all the keys, URLs, and check dates from the keys JSON file.
        :return: Dict containing the keys, URLs, and check dates.
        """
        path = os.environ['KEY_PATH']
        with open(path, 'r') as j:
            return json.loads(j.read())

    def get_key(self):
        """
        Key a key and url pair that is not emptied this month.
        If the key was never used, the checked date is empty and can be used.
        If the key was emptied but was empties last month, it can be used again.
        :return: Key and URL pair that is still available.
        """
        keys = self._read_keys()
        for index, key_pair in enumerate(keys):
            key, url, checked = key_pair.values()
            if not checked:  # Never checked this key.
                self.current_key = index
                return key, url
            key_date = datetime.strptime(checked, '%Y-%m-%d')
            today = date.today()
            if key_date.month < today.month:  # Key was empty last month. Values were reset.
                self.current_key = index
                return key, url
        print('No more keys left. All were used this month.')
        os.close(0)

    def set_key_empty(self):
        """
        Set current key in use to the current date. This key is emptied and cannot be used for this
        month anymore.
        """
        keys = self._read_keys()
        key = keys[self.current_key]
        key['checked'] = date.today().strftime('%Y-%m-%d')
        keys[self.current_key] = key
        with open(os.environ['KEY_PATH'], 'w') as j:
            j.write(json.dumps(keys, indent=2))

    def profile(self, data):
        """
        Create a profile for the user message data provided as parameter.
        :param data: Message data of the user.
        :return: Json profile of the user.
        """
        try:
            return self.personality_insights.profile(
                content=data,
                accept='application/json',
                content_type='text/plain',
                raw_scores=True
            ).get_result()
        except ApiException as ex:
            if ex.code == 400:
                if 'The number of words' in ex.message:
                    return int(ex.message[20:].split()[0])
                if 'The input you provided' in ex.message:
                    return int(ex.message[31:].split()[0])

            print("Method failed with status code " + str(ex.code) + ": " + ex.message)
            if ex.code == 403:
                self.set_key_empty()
                raise ex
            return {}
        except ConnectionError as ex:
            print("Method failed with: " + str(ex) + "\nEntering temporary sleep mode.")
            sleep(5)
            return {}
