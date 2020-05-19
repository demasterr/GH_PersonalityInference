"""
Module for all analyses.
"""
import json
import math
import os
import pathlib
import shutil
import re
from abc import ABC, abstractmethod
from multiprocessing import Pool
from time import sleep

from notify_run import Notify
from tqdm import tqdm

from connection import MySQLConnector, IBMConnector
from liwc_methods import LIWCScores
from parsing import Parser
from parsing import StemParser


class Analysis(ABC):
    """
    Abstract class for analysis types.
    """

    def __init__(self, config):
        """
        Abstract analysis instance.
        :param config: Configuration settings for the analysis.
        """
        self.max_persons = config['max_users']
        self.table = config['table']
        self.minimum_words = config['minimum_words']
        self.notify = Notify()
        self.config = config
        if not self.notify.config_file_exists:
            print(
                'Notify not registered yet. No messages send to device.\n'
                'Please consider registering using "notify-run register" in the console.')
        else:
            self.notify.send("Starting {}, messaging properly setup.".format(type(self).__name__))

    def send_notification(self, message):
        """
        Send a notification to an external device or browser. This could help you keep track of
        progress when away or distracted.
        :param message: Message send to notify_run.
        """
        if self.notify.config_file_exists:
            self.notify.send(message)

    @abstractmethod
    def execute(self):
        """
        Abstract method to indicate the start of an analysis.
        """
        # Must be overriden.

    @abstractmethod
    def _process_row(self, row):
        """
        Abstract method for processing a single row of data.
        :param row: Row to be processed.
        """
        # Must be overridden.


class Preprocessing(Analysis):
    """
    Pre-processing analysis. Parsing is initiated here.
    """

    def __init__(self, config):
        """
        Initialize preprocessing analysis.
        :param config: Configuration settings for the Preprocessing step.
        """
        self.skipped = 0
        self.user_filter = config['filter']
        self.truncate = config['truncate']
        self.stemming = config['stemming']
        super().__init__(config)

    def execute(self):
        """
        Pre-process the user messages for the other analyses.
        """
        # Execute SQL query for commits
        query = """
                SELECT user_id, body
                FROM commit_comments
                UNION
                SELECT user_id, body
                FROM pull_request_comments
                ORDER BY user_id
                """
        result_commits = MySQLConnector.execute(query)
        print("Fetched user comments from database. Starting parse process..")
        self._process_stepwise(result_commits)
        print("Preprocessing skipped {} users.\n\tThese users did not meet the requirement of a "
              "minimum of {} words.".format(self.skipped, self.minimum_words))
        self.send_notification("I'm done Preprocessing.")

    def _process_stepwise(self, data):
        """
        Stepwise processing to make it possible for each row to be parsed as an individual comment
        instead of all comments combined as a single message.

        In this way, parsing can be done on incomplete code blocks. A valid code block does not
        have to end with three ticks if the end of the code block is the end of the comment.
        :param data: Data extracted from the database query containing all comments of users.
        """
        if not data:
            print("Nothing found to process.")
        else:
            users_processed = 0
            current_user = -1
            parser = Parser()
            total = ('', '', 0)
            skip = False
            with tqdm(total=len(data)) as pbar:
                for row in data:
                    # Stop if the maximum of users is set and reached.
                    if 0 < self.max_persons == users_processed:
                        break
                    (user_id, comment) = row
                    # If a new user arises, set the new user_id.
                    if current_user != user_id:
                        # Insert the processed user in the database and reset variables.
                        if current_user > 0:
                            self._process_row((current_user, total))
                            users_processed += 1
                        total = ('', '', 0)
                        current_user = user_id
                        skip = current_user in self.user_filter
                    if not skip:
                        parsed_text, word_count = parser.parse(comment)
                        if word_count > 0:
                            stemmed = total[1] + ' ' + StemParser().stem(
                                parsed_text) if self.stemming else ''
                            total = total[0] + ' ' + parsed_text, stemmed, total[2] + word_count
                    pbar.update(1)
            MySQLConnector.commit()

        # Close the SQL connnection.
        MySQLConnector.close()

    def _process_row(self, row):
        """
        Process and analyze the query results.
        :param row: Result fetched from the MySQL database containing users and their messages.
        """
        user_id = row[0]
        (parsed_text, stemmed_text, word_count) = row[1]
        if word_count >= self.minimum_words:
            # Add the user to the database, either with stemmed parsing or no stemming.
            if self.stemming:
                MySQLConnector.insert(self.table,
                                      '(id, comments, comments_stemmed, word_count)',
                                      (user_id, parsed_text[:self.truncate],
                                       stemmed_text[:self.truncate],
                                       min(word_count, self.truncate)))
            else:
                MySQLConnector.insert(self.table,
                                      '(id, comments, word_count)',
                                      (user_id, parsed_text[:self.truncate],
                                       min(word_count, self.truncate)))
        else:
            self.skipped += 1


class LIWC(Analysis):
    """
    LIWC analysis instance.
    """
    LIWC_SETTINGS_PATH = './out/liwc.json'

    def __init__(self, config):
        """
        Create an LIWC analysis with the specified significance.
        :param config: Configuration for the LIWC step.
        """
        self.signf = config['significance']
        self.frequency_file = config['frequency_file']
        self.path = os.environ['LIWC_INPUT']
        super().__init__(config)

    def execute(self):
        """
        Initiate LIWC analysis.
        """
        print("Starting LIWC preprocessing step..")
        pathlib.Path(self.path).mkdir(parents=True, exist_ok=True)

        feature = self.get_feature(self.config['base_table'])
        liwcs = [LIWCScores(self.signf, version=liwc_version['version'],
                            author=liwc_version['author'], feature=feature)
                 for liwc_version in self.config['liwc_versions']]

        if all(os.path.exists(liwc_version.path) for liwc_version in liwcs):
            print("Found earlier processed LIWC input files. Program continues using these files..")
        elif os.listdir(self.path) and self.same_settings():
            print("Found the same settings of an earlier run."
                  "Program continues using these files..")
            input("Start the LIWC application and generate CSV files"
                  "\n\nPress ENTER after the CSV file is generated to continue..")
        else:
            self.generate_input_files()
            self.send_notification("Please start processing LIWC files!")
            input("Done processing LIWC input files. Start the LIWC application and generate CSV "
                  "files.\n\nPress ENTER after the CSV file is generated to continue..")
        # Read and process the LIWC CSV files.
        for liwc_version in liwcs:
            self._process_csv(liwc=liwc_version)
        self.send_notification("I'm done processing LIWC scores.")

    def generate_input_files(self):
        """
        Generate the input text files so LIWC can start generating scores.
        """
        # Clear the input directory and the settings file.
        self.clear_directory()
        # If frequency filter is enabled, calculated n most frequent words and words across n
        # different users. This filter is disabled on default.
        if self.frequency_file:
            print('Removing words that are not in frequent list')
            with open(self.frequency_file, 'r') as file:
                frequent_words = set(json.loads(file.readlines()[0]))
        else:
            frequent_words = {}

        print('Querying users and respective comments..')
        # Read in the users and their respective comments and form input files for LIWC.
        query = "SELECT id,comments FROM {} WHERE word_count >= {}".format(self.table,
                                                                           self.minimum_words)
        result = MySQLConnector.execute(query, limit=self.max_persons)
        print('Starting to process LIWC input files..')
        regex = re.compile('[^a-zA-Z ]')
        for row in tqdm(result):
            self._write_input_file(row, frequent_words, regex)

    def _write_input_file(self, row, frequent_words, regex):
        """
        Prepare input files for the LIWC application, which should be used externally.
        :param row: Row containing the user and the comments of the user.
        :param frequent_words: The frequent words found among all comments of users.
        """
        (user_id, text) = row
        text = text.replace('*code*', '')
        if frequent_words:
            comment = regex.sub('', text)
            out = ' '.join([word for word in str(comment).split() if word in frequent_words])
        else:
            out = text
        # Create a txt file of the user for LIWC processing.
        path = self.path + '{}.txt'.format(user_id)
        with open(path, 'w', encoding='utf-8') as file:
            file.write(out)

    def _process_row(self, row):
        """
        Process a single user row.
        :param row: Data row from the database containing the parsed user data.
        """
        (user_id, (scores_row, scores_row_norm), author) = row
        query = r"""UPDATE {table}
                SET liwc_openness_{author}={},
                    liwc_conscientiousness_{author}={},
                    liwc_extraversion_{author}={},
                    liwc_agreeableness_{author}={},
                    liwc_neuroticism_{author}={},
                    liwc_filled_{author}=1,
                    liwc_openness_{author}_raw={},
                    liwc_conscientiousness_{author}_raw={},
                    liwc_extraversion_{author}_raw={},
                    liwc_agreeableness_{author}_raw={},
                    liwc_neuroticism_{author}_raw={}
                WHERE id={}
                """.format(*scores_row_norm, *scores_row, user_id, table=self.table, author=author)
        MySQLConnector.execute(query, has_return=False)

    def _process_csv(self, liwc):
        """
        Process the LIWC scores from the CSV and save the results in the database.
        """
        print("Processing correlation of {} using LIWC{}".format(liwc.author, liwc.version))
        # Retrieve the raw scores and the normalized scores for the author and LIWC version.
        scores_df, norm_scores_df = liwc.get_liwc_scores()

        # Process the LIWC scores.
        for i in tqdm(range(0, len(scores_df))):
            row_scores = scores_df.iloc[i]
            row_scores_norm = norm_scores_df.iloc[i]
            user_id = row_scores.name
            assert user_id == row_scores_norm.name
            self._process_row((user_id, (tuple(row_scores), tuple(row_scores_norm)), liwc.author))

        MySQLConnector.commit()

    def same_settings(self) -> bool:
        """
        :return boolean indicating if the settings of an earlier run are the same.
        """
        # If the file exists, check if the fields are equal. Else return false.
        if os.path.exists(self.LIWC_SETTINGS_PATH):
            with open(self.LIWC_SETTINGS_PATH, 'r') as file:
                data = json.load(file)
                return self.max_persons == data['max_persons'] and self.table == data[
                    'table'] and self.signf == data['signf'] and self.frequency_file == data[
                           'frequency_file'] and self.minimum_words == data['minimum_words']
        return False

    def save_settings(self):
        """
        Save the settings currently used for the LIWC.
        """
        settings = {'max_persons': self.max_persons, 'table': self.table, 'signf': self.signf,
                    'frequency_file': self.frequency_file, 'minimum_words': self.minimum_words}
        with open(self.LIWC_SETTINGS_PATH, 'w') as file:
            json.dump(settings, file)

    def clear_directory(self):
        """
        If the settings used earlier were different, clear the directory and the settings.
        """
        # Clear the LIWC input directory.
        shutil.rmtree(self.path)  # Delete the folder.
        os.mkdir(self.path)  # Create a new empty folder.
        # Clear the settings file.
        if os.path.exists(self.LIWC_SETTINGS_PATH):
            os.remove(self.LIWC_SETTINGS_PATH)
        self.save_settings()  # Create new settings file.
        print('Different settings used earlier. Refreshed input files.')

    def get_feature(self, base_table):
        """
        If a base table is specified, get the feature that is currently processed.
        This enables to save under different names with the LIWC results.
        For example, LIWC2007 Results - at.csv is the result for the <base_table>_at database.
        :param base_table: Base name for the table, all tables have this prefix.
        :return: Feature that comes after the base table name.
        """
        if not base_table:
            return None
        table_split = self.table.split(base_table + '_')
        if len(table_split) == 2 and table_split[1]:
            return table_split[1]
        if len(table_split) > 2:
            print('Invalid name found for table. Base table is contained multiple times!')
        return None


class LIWCQuick(LIWC):
    def execute(self):
        features = self.get_feature(self.config['base_table'])
        print("Found features to process: {}".format(features))

        if not features:
            print("No base table is defined. Please set 'base_table' in the config.json"
                  "to continue.\nProgram aborted..")
            return

        for feature in features:
            self.table = self.config['base_table'] + "_" + feature
            for liwc_version in self.config['liwc_versions']:
                liwc_scores = LIWCScores(self.signf, version=liwc_version['version'],
                                         author=liwc_version['author'],
                                         feature=feature)
                print('Starting to process {} for {}'.format(liwc_scores.author, feature))
                self._process_csv(liwc=liwc_scores)
        self.send_notification("I'm done processing LIWC scores for {}.".format(features))

    def get_feature(self, base_table):
        if not base_table:
            return None
        csv_files = {file for file in os.listdir(LIWCScores.BASE_PATH) if file.endswith('.csv')}
        quick_file_ext = self.config['quick_tables']
        kept_ext, skipped_files = [], []
        years = [version['version'] for version in self.config['liwc_versions']]
        for ext in quick_file_ext:
            has_missing = False
            for year in years:
                filename = "LIWC{year} Results - {ext}.csv".format(year=year, ext=ext)
                if filename not in csv_files:
                    skipped_files.append(filename)
                    has_missing = True
            if not has_missing:
                kept_ext.append(ext)
        print("Missing the following LIWC .csv score files:\n {}".format(skipped_files))
        return kept_ext


class PersonalityInsights(Analysis):
    """
    IBM Watson Personality Insights analysis instance.
    """

    def __init__(self, config):
        """
        Create an LIWC analysis with the specified significance.
        :param max_persons: Maximum number of persons to process.
        :param table: MySQL table name.
        :param minimum_words: Minimum words to be included in further analysis.
        :param chunk_size: Data is chunked in this size.
        """
        self.chunk_size = config['chunk_size']
        super().__init__(config)

    def execute(self):
        """
        Initiate IBM Watson Personality Insights analysis.
        """
        self.execute_chunked(step=self.get_steps(), chunk_size=self.chunk_size)

        self.send_notification("I'm done processing PI")

    def get_steps(self):
        query = """
            SELECT COUNT(id)
            FROM {}
            WHERE pi_filled=0 AND word_count >= {}
            """.format(self.table, self.minimum_words)
        result = MySQLConnector.execute(query, limit=self.max_persons)
        steps = math.ceil(result[0][0] / self.chunk_size)
        print("Processing {} users with chunk-size {}".format(result[0][0], self.chunk_size))
        return steps

    def execute_chunked(self, step, chunk_size=200, retry=False):
        assert chunk_size > 0
        # Select all rows that were not processed before.
        query = """
            SELECT id, comments
            FROM {}
            WHERE pi_filled=0 AND word_count >= {}
            LIMIT {}
            """.format(self.table, self.minimum_words, chunk_size)
        print("Processing steps left: {}".format(step))
        result = MySQLConnector.execute(query)
        data_len = self._process(result)

        if data_len == chunk_size:
            sleep(8)
            self.execute_chunked(step - 1, chunk_size)
        elif data_len == -1 and not retry:
            print('Crashed during current chunk. Retry will start soon after a short break.')
            sleep(15)
            self.execute_chunked(step=self.get_steps(), chunk_size=self.chunk_size, retry=True)
        else:
            if data_len == -1:
                self.send_notification("My PI process just crashed.")
            # Close the SQL connnection.
            MySQLConnector.close()

    def _process(self, data):
        """
        Process and analyze the query results.
        :param data: Result fetched from the MySQL database.
        """
        if not data:
            print("Nothing found to process.")
        else:
            # Create process bar.
            with tqdm(total=len(data)) as pbar:
                # Initiate multi-threaded process.
                pool = Pool(processes=self.config['threads'])

                # Process each row of the fetched data, commit, and wait until all finished.
                try:
                    for _, _ in tqdm(enumerate(pool.imap_unordered(self._process_row, data))):
                        pbar.update()
                except Exception as exception:
                    print('Exception during threading.\n{}'.format(str(exception)))
                    return -1
                pool.close()
                pool.join()
        return len(data)

    def _process_row(self, row):
        """
        Process and analyze a single user using Personality Insights.
        :param row: User information fetched from the MySQL database.
        """
        (user, text) = row
        text = text.replace('*code*', '')

        # Connect to the IBM service and fetch a single user profile.
        try:
            connector = IBMConnector()
            result = connector.profile(text)
        except Exception as exception:
            print('PI process failed with exception: {}'.format(exception))
            raise exception
        if isinstance(result, int):
            query = """UPDATE {} SET word_count={} WHERE id={}""".format(self.table, result, user)
            MySQLConnector.execute(query, has_return=False)
            MySQLConnector.commit()
            print('Updated word_count for: ' + str(user))
        elif result:
            # Convert the fetched profile to a database result.
            self._process_result(user, result['personality'], result['word_count'])
        else:
            print('Skipped user: ' + str(user))

    def _process_result(self, user, profile, word_count):
        """
        Process Json result of the PI service.
        :param profile: Profile containing the Big-Five personality scores.
        """
        # Percentiles range 0-100 instead of 0-1 and round to a fixed decimal point of 15 or less.
        profile_percentiles = [round(trait['percentile'] * 100, 15) for trait in profile]
        # Round to a fixed decimal point of 15 or less.
        profile_raw = [round(trait['raw_score'], 15) for trait in profile]
        query = """
        UPDATE {table}
        SET word_count={},
            pi_openness={},
            pi_conscientiousness={},
            pi_extraversion={},
            pi_agreeableness={},
            pi_neuroticism={},
            pi_openness_raw={},
            pi_conscientiousness_raw={},
            pi_extraversion_raw={},
            pi_agreeableness_raw={},
            pi_neuroticism_raw={},
            pi_filled=1
        WHERE id={}""".format(word_count, *profile_percentiles, *profile_raw, user,
                              table=self.table)
        MySQLConnector.execute(query, has_return=False)
        MySQLConnector.commit()
