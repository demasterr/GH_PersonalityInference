"""
Module for loading configuration settings of the whole project.
"""
import json
import os
from typing import Dict


class Config:
    """
    Class for configuration of full project fetched from 'config.json'.
    """
    _CONFIG: Dict = {}

    @staticmethod
    def load():
        """
        Load the configuration settings from 'config.json'.
        """
        with open('config.json') as json_config:
            Config._CONFIG = json.load(json_config)
        Config._CONFIG['password'] = os.environ['PASSWORD']

    @staticmethod
    def get_sql_config() -> Dict:
        """
        Get the configuration dict specifically for MySQL connections.
        Keys that are not specifically for MySQL connection are ruled out.
        :return: Filtered configuration settings for MySQL connections.
        """
        if not Config._CONFIG:
            Config.load()  # Config was not loaded.
        sql_keys = ['user', 'password', 'host', 'database', 'raise_on_warnings']
        return {k: Config._CONFIG[k] for k in sql_keys if k in Config._CONFIG}

    @staticmethod
    def get_config() -> Dict:
        """
        Retrieve the full configuration settings.
        :return: Full configuration settings of the project.
        """
        if not Config._CONFIG:
            Config.load()
        return Config._CONFIG

    @staticmethod
    def get_setting(setting: str):
        """
        Get a setting from the configuration.
        :param setting: Setting name in string format.
        :return: Value of the setting in the configuration.
        """
        return Config.get_config()[setting]
