"""
Test classes for the connection module.
"""
import unittest
from unittest.mock import Mock
from unittest.mock import patch

from mysql.connector import MySQLConnection

from connection import MySQLConnector


class MySQLConnectorTest(unittest.TestCase):
    """
    Tests for the MySQLConnector class.
    """

    def setUp(self) -> None:
        """
        Setup before each test.
        """
        self.mock_mysql_connection = Mock(MySQLConnection)

    @patch('connection.MySQLConnector._connect')
    def test_get_connection_connect(self, func_connect):
        """
        Test if the get_connection properly calls _connect and returns the connection afterward.
        """
        connector = MySQLConnector()
        result = connector.get_connection()
        self.assertEqual(None, result)

    def test_get_connection_connected(self):
        """
        Test if the get_connection properly returns an existing connection, if it exists.
        """
        with patch('connection.MySQLConnector._CONNECTION', self.mock_mysql_connection):
            connector = MySQLConnector()
            result = connector.get_connection()
            self.assertEqual(self.mock_mysql_connection, result)

    def test_close_connected(self):
        """
        Test if the close properly calls the close on the connection, if a connection exists.
        """
        with patch('connection.MySQLConnector._CONNECTION', self.mock_mysql_connection):
            connector = MySQLConnector()
            connector.close()
            self.mock_mysql_connection.close.assert_called_once_with()

    @patch('connection.MySQLConnector._CONNECTION', None)
    def test_close_not_connected(self):
        """
        Test if the close properly skips if there is no connection to close.
        """
        connector = MySQLConnector()
        connector.close()
        self.mock_mysql_connection.close.assert_not_called()
