"""
Test classes for the config module.
"""
import unittest
from unittest.mock import patch

from config import Config


class TestConfig(unittest.TestCase):
    """
    Tests for the Config class.
    """
    mocked_config = {'user': 'john', 'password': 'doe', 'host': '127.0.0.1', 'database': 'db',
                     'raise_on_warnings': True, "max_users": 100, "pi_api_url": "https://url.com",
                     "step": "Testing", "minimum_words": 10}

    @patch('config.Config._CONFIG', mocked_config)
    def test_sql_config(self):
        """
        Test if the SQL config can properly be retrieved by using get_sql_config.
        """
        expected = {'user': 'john', 'password': 'doe', 'host': '127.0.0.1', 'database': 'db',
                    'raise_on_warnings': True}

        config = Config()
        result = config.get_sql_config()
        self.assertEqual(expected, result)

    @patch('config.Config._CONFIG', mocked_config)
    def test_get_config(self):
        """
        Test if the get_config properly returns the whole config.
        """
        expected = self.mocked_config

        config = Config()
        result = config.get_config()
        self.assertEqual(expected, result)

    @patch('config.Config._CONFIG', mocked_config)
    def test_get_setting(self):
        """
        Test if a single setting can properly be retrieved using get_config.
        """
        expected = '127.0.0.1'

        config = Config()
        result = config.get_setting('host')
        self.assertEqual(expected, result)
