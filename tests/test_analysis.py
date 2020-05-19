"""
Test classes for the analysis module.
"""
import unittest

from analysis import Preprocessing


class TestPreprocessing(unittest.TestCase):
    """
    Tests for the Preprocessing class.
    """

    def test_skip_word_count_low(self):
        """
        Test if a row is skipped if the word_count is too low.
        """
        feed = (1, ('', '', 0))
        expected = 1  # Expected skip value.

        analysis = Preprocessing(max_persons=0, table='some_table', minimum_words=1, user_filter=[],
                                 truncate=10000, stemming=False)
        analysis._process_row(feed)
        self.assertEqual(expected, analysis.skipped)
