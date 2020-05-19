"""
Module for LIWC related functionality.
"""
import os
from typing import Tuple

import numpy as np
import pandas as pd
import swifter
from tqdm import tqdm
import pysentiment as ps

from connection import MySQLConnector


class LIWCScores:
    """
    Class responsible for the calculation of LIWC analysis scores.
    """
    BASE_PATH = "./out/LIWC/output/"

    # Correlation matrix for signficance level p<0.05.
    corr_05 = np.array([[0, 0, -0.21, 0.11, 0],
                        [0.12, 0, -0.16, 0, 0],
                        [0, 0.11, -0.1, 0.18, 0],
                        [0.1, 0, -0.19, 0.08, 0],
                        [-0.15, 0.16, -0.12, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0.11, 0, -0.13, 0, -0.17],
                        [0, 0, -0.11, 0, -0.09],
                        [-0.11, 0, 0.2, 0, 0.09],
                        [0, 0, 0.17, 0, 0],
                        [0, -0.12, -0.08, 0.11, 0],
                        [0, 0.09, -0.12, 0, 0],
                        [0, 0.1, -0.15, 0.18, 0],
                        [0, 0.11, -0.11, 0.14, 0],
                        [0.08, 0, 0, 0.15, 0.16],
                        [0.16, 0, 0, -0.15, -0.18],
                        [0.17, 0, 0, 0, 0],
                        [0.13, 0, 0, -0.23, -0.19],
                        [0.1, 0, 0, 0, -0.11],
                        [0.13, 0, -0.09, 0, -0.11],
                        [0.11, -0.09, 0, -0.11, -0.12],
                        [0, 0, 0, 0, 0],
                        [0.13, 0, -0.12, 0, -0.13],
                        [0.09, -0.13, 0, 0, 0],
                        [0.12, -0.11, 0, 0, -0.1],
                        [0.13, 0.1, 0, 0, -0.1],
                        [0, 0.09, -0.11, 0, -0.1],
                        [0, 0, 0, 0.09, 0],
                        [0, 0.12, -0.08, 0, -0.12],
                        [0.1, 0, 0, 0.1, 0],
                        [0, 0.15, -0.14, 0.13, 0],
                        [0, 0.13, 0, 0, 0],
                        [-0.08, 0.15, -0.14, 0.15, 0],
                        [-0.08, 0.15, 0, 0.11, 0],
                        [0, 0.09, -0.17, 0.19, 0],
                        [0, 0.13, -0.09, 0, -0.12],
                        [0, 0, -0.22, 0.12, 0.09],
                        [0, 0, -0.16, 0.1, 0],
                        [0, 0, -0.16, 0, 0],
                        [0, 0, 0, 0, 0],
                        [-0.09, 0, -0.11, 0.16, 0],
                        [-0.1, 0.09, -0.15, 0.11, 0.09],
                        [0, 0, -0.11, 0.11, 0],
                        [0, 0.09, 0.11, 0.18, 0],
                        [0.1, 0, 0, 0, -0.16],
                        [0, 0, -0.22, 0.14, 0],
                        [0, -0.12, 0, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, -0.08, 0, 0, 0],
                        [0, -0.09, 0, 0, 0.14],
                        [0, 0.08, -0.17, 0.15, 0],
                        [0, 0, -0.2, 0.19, 0],
                        [0, 0, -0.14, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.13, 0, 0.08, -0.11],
                        [0, 0, 0, -0.11, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.11, 0, 0, 0],
                        [0, 0, 0.15, -0.13, -0.12],
                        [0, 0.14, -0.09, 0.09, 0],
                        [0, 0.1, 0, 0.09, 0],
                        [0, 0.17, 0, 0.08, 0],
                        [0, 0, -0.15, 0, 0],
                        [0.1, 0, -0.14, 0.11, 0],
                        [0, 0, -0.2, 0, 0],
                        [0.11, 0, 0, -0.21, -0.14]])
    # Correlation matrix for signficance level p<0.01.
    corr_01 = np.array([[0, 0, -0.21, 0.11, 0],
                        [0.12, 0, -0.16, 0, 0],
                        [0, 0.11, 0, 0.18, 0],
                        [0, 0, -0.19, 0, 0],
                        [-0.15, 0.16, -0.12, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0.11, 0, -0.13, 0, -0.17],
                        [0, 0, -0.11, 0, 0],
                        [-0.11, 0, 0.2, 0, 0],
                        [0, 0, 0.17, 0, 0],
                        [0, -0.12, 0, 0, 0],
                        [0, 0, -0.12, 0, 0],
                        [0, 0, -0.15, 0.18, 0],
                        [0, 0, -0.11, 0.14, 0],
                        [0, 0, 0, 0.15, 0.16],
                        [0.16, 0, 0, -0.15, -0.18],
                        [0.17, 0, 0, 0, 0],
                        [0.13, 0, 0, -0.23, -0.19],
                        [0, 0, 0, 0, 0],
                        [0.13, 0, 0, 0, -0.11],
                        [0.11, 0, 0, -0.11, -0.12],
                        [0, 0, 0, 0, 0],
                        [0.13, 0, -0.12, 0, -0.13],
                        [0, -0.13, 0, 0, 0],
                        [0.12, 0, 0, 0, 0],
                        [0.13, 0, 0, 0, 0],
                        [0, 0, -0.11, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.12, 0, 0, -0.12],
                        [0, 0, 0, 0, 0],
                        [0, 0.15, -0.14, 0.13, 0],
                        [0, 0.13, 0, 0, 0],
                        [0, 0.15, -0.14, 0.15, 0],
                        [0, 0.15, 0, 0.11, 0],
                        [0, 0, -0.17, 0.19, 0],
                        [0, 0.13, 0, 0, -0.12],
                        [0, 0, -0.22, 0.12, 0],
                        [0, 0, -0.16, 0, 0],
                        [0, 0, -0.16, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0, -0.11, 0.16, 0],
                        [0, 0, -0.15, 0.11, 0],
                        [0, 0, -0.11, 0.11, 0],
                        [0, 0, 0.11, 0.18, 0],
                        [0, 0, 0, 0, -0.16],
                        [0, 0, -0.22, 0.14, 0],
                        [0, -0.12, 0, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0.14],
                        [0, 0, -0.17, 0.15, 0],
                        [0, 0, -0.2, 0.19, 0],
                        [0, 0, -0.14, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.13, 0, 0, -0.11],
                        [0, 0, 0, -0.11, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.11, 0, 0, 0],
                        [0, 0, 0.15, -0.13, -0.12],
                        [0, 0.14, 0, 0, 0],
                        [0, 0, 0, 0, 0],
                        [0, 0.17, 0, 0, 0],
                        [0, 0, -0.15, 0, 0],
                        [0, 0, -0.14, 0.11, 0],
                        [0, 0, -0.2, 0, 0],
                        [0.11, 0, 0, -0.21, -0.14]])
    # Correlation matrix for signficance level p<0.001.
    corr_001 = np.array([[0, 0, -0.21, 0, 0],
                         [0, 0, -0.16, 0, 0],
                         [0, 0, 0, 0.18, 0],
                         [0, 0, -0.19, 0, 0],
                         [-0.15, 0.16, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, -0.17],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0.2, 0, 0],
                         [0, 0, 0.17, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, -0.15, 0.18, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0.15, 0.16],
                         [0.16, 0, 0, -0.15, -0.18],
                         [0.17, 0, 0, 0, 0],
                         [0, 0, 0, -0.23, -0.19],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0.15, -0.14, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0.15, -0.14, 0.15, 0],
                         [0, 0.15, 0, 0, 0],
                         [0, 0, -0.17, 0.19, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, -0.22, 0, 0],
                         [0, 0, -0.16, 0, 0],
                         [0, 0, -0.16, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0.16, 0],
                         [0, 0, -0.15, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0.18, 0],
                         [0, 0, 0, 0, -0.16],
                         [0, 0, -0.22, 0.14, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0.14],
                         [0, 0, -0.17, 0.15, 0],
                         [0, 0, -0.2, 0.19, 0],
                         [0, 0, -0.14, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0, 0.15, 0, 0],
                         [0, 0.14, 0, 0, 0],
                         [0, 0, 0, 0, 0],
                         [0, 0.17, 0, 0, 0],
                         [0, 0, -0.15, 0, 0],
                         [0, 0, -0.14, 0, 0],
                         [0, 0, -0.2, 0, 0],
                         [0, 0, 0, -0.21, -0.14]])

    #    N  E  O  A  C
    corr_05_golbeck = np.array([
        # [0, 0, 0, 0, 0],  # pronoun
        # [0, 0, 0, 0, 0],  # i
        # [0, 0, 0, 0, 0],  # we
        # [0, 0, 0, 0, 0],  # ppron
        [0, 0, 0, 0.364, 0.252],  # you
        # [0, 0, 0, 0, 0],  # shehe
        [0, 0, 0, 0, -0.374],  # negate
        # [0, 0, 0, 0, 0],  # assent
        [0, 0, 0.396, 0, 0],  # article
        # [0, 0, 0, 0, 0],  # preps
        # [0, 0, 0, 0, 0],  # number
        # [0, 0, 0, 0, 0],  # affect
        # [0, 0, 0, 0, 0],  # posemo
        [0, 0, 0, 0, -0.268],  # negemo
        # [0, 0, 0, 0, 0],  # anx
        # [0, 0, 0, 0, 0],  # anger
        [0, 0, 0, 0, -0.253],  # sad
        [0, 0, 0, 0, -0.244],  # cogmech
        [0, 0, 0.264, -0.258, 0],  # cause
        # [0, 0, 0, 0, 0],  # insight
        [0, 0, 0, 0, -0.292],  # discrep
        # [0, 0, 0, 0, 0],  # inhib
        # [0, 0, 0, 0, 0],  # tentat
        [0, 0, 0.347, 0, 0],  # certain
        # [0, 0, 0, 0, 0],  # percept
        # [0, 0, 0, 0, 0],  # see
        [0.335, 0, 0, 0, 0],  # hear
        [0.244, 0, 0, 0, -0.236],  # feel
        [0, 0.262, 0, 0, 0],  # social
        # [0, 0, 0, 0, 0],  # friend
        [0, 0.338, 0, 0, 0],  # family
        [0, 0, 0.251, 0, 0],  # humans
        # [0, 0, 0, 0, 0],  # time
        # [0, 0, 0, 0, 0],  # past
        # [0, 0, 0, 0, 0],  # present
        [0, 0, 0, 0, -0.286],  # future
        # [0, 0, 0, 0, 0],  # space
        # [0, 0, 0, 0, 0],  # incl
        # [0, 0, 0, 0, 0],  # excl
        # [0, 0, 0, 0, 0],  # motion
        [0, 0, 0.426, 0, 0.330],  # work
        [0, 0, 0, -0.240, 0],  # achieve
        # [0, 0, 0, 0, 0],  # leisure
        # [0, 0, 0, 0, 0],  # home
        [0, 0, 0, -0.259, 0],  # money
        [0.383, 0, 0, 0, 0],  # relig
        [0, 0, 0, 0, -0.332],  # death
        [0, 0, -0.239, 0, 0],  # bio
        [0, 0, -0.299, 0, 0],  # body
        # [0, 0, 0, 0, 0],  # sexual
        [0, 0, 0, 0.247, 0],  # ingest
        # [0, 0, 0, 0, 0],  # swear
        [0, 0, 0, 0, -0.284],  # auxverb
        [0, 0, 0.238, 0, 0],  # quant
        [0, -0.277, 0, 0, 0],  # health
        [0, 0, 0, 0, -0.272],  # filler
        [0, 0, 0, 0, -0.24],  # Comma
        [0, 0, 0, 0, 0.322],  # Colon
        [0, 0.263, 0, 0, 0],  # QMark
        [0.317, 0, -0.295, 0, 0.260],  # Exclam
        [0, -0.254, -0.302, 0, 0]])  # Parenth

    def __init__(self, signf=0.05, version=2001, author='yarkoni', feature=None):
        """
        Constructor for creating an LIWC calculation instance.
        :param signf: Significance level (p <0.05, <0.01, or <0.001).
        :param version: LIWC Version (2001, 2007, or 2015).
        :param author: LIWC correlation values author (yarkoni/golbeck).
        :param feature: Feature for which the LIWC is executed
        """
        self.signf = signf
        self.version = version
        self.author = author
        if feature:
            self.path = self.BASE_PATH + 'LIWC{} Results - {}.csv'.format(self.version, feature)
        else:
            self.path = self.BASE_PATH + 'LIWC{} Results.csv'.format(self.version)

    def get_neuroticism(self, cat_scores) -> float:
        """
        Calculate the neuroticism of the user.
        :param cat_scores: LIWC category scores.
        :return: Big Five personality score for Neuroticism.
        """
        return self._get_significance_matrix()[:, 0].dot(cat_scores)

    def get_extraversion(self, cat_scores) -> float:
        """
        Calculate the extraversion of the user.
        :param cat_scores: LIWC category scores.
        :return: Big Five personality score for Extraversion.
        """
        return self._get_significance_matrix()[:, 1].dot(cat_scores)

    def get_openness(self, cat_scores) -> float:
        """
        Calculate the openness of the user.
        :param cat_scores: LIWC category scores.
        :return: Big Five personality score for Openness.
        """
        return self._get_significance_matrix()[:, 2].dot(cat_scores)

    def get_agreeableness(self, cat_scores) -> float:
        """
        Calculate the agreeableness of the user.
        :return: Big Five personality score for Agreeableness.
        """
        return self._get_significance_matrix()[:, 3].dot(cat_scores)

    def get_conscientiousness(self, cat_scores) -> float:
        """
        Calculate the "conscientiousness" of the user.
        :param cat_scores: LIWC category scores.
        :return: Big Five personality score for Conscientiousness.
        """
        return self._get_significance_matrix()[:, 4].dot(cat_scores)

    def get_big_five_scores(self, cat_scores) -> Tuple[float, float, float, float, float]:
        """
        Get all of the Big-Five scores as a single tuple.
        :return: Tuple containing the Big-Five scores in OCEAN order.
        """
        return self.get_openness(cat_scores), self.get_conscientiousness(
            cat_scores), self.get_extraversion(cat_scores), self.get_agreeableness(
            cat_scores), self.get_neuroticism(cat_scores)

    def _get_significance_matrix(self) -> np.array:
        """
        Get the significance level matrix. I.e., get the matrix containing the correlations with
        the right significance level.

        Note that for author=golbeck, the signf variable does not have any influence as only p<0.05
        is available.
        :return: Correlation matrix with the right significance.
        """
        if self.author == 'yarkoni':
            if self.signf == 0.05:
                return self.corr_05
            if self.signf == 0.01:
                return self.corr_01
            return self.corr_001
        return self.corr_05_golbeck  # Author golbeck

    @staticmethod
    def _get_columns(version, author):
        """
        Get the columns that below to the given LIWC version. Different LIWC versions have different
        column names and different columns available.
        :param version: LIWC version for which columns are selected.
        :param author: Author versino for which the columns are selected.
        :return: Columns available for the given LIWC version.
        """
        # Version 2001
        if version == 2001:
            return ['Pronoun', 'I', 'We', 'Self', 'You', 'Other', 'Negate', 'Assent', 'Article',
                    'Preps', 'Number', 'Affect', 'Posemo', 'Posfeel', 'Optim', 'Negemo', 'Anx',
                    'Anger', 'Sad', 'Cogmech', 'Cause', 'Insight', 'Discrep', 'Inhib', 'Tentat',
                    'Certain', 'Senses', 'See', 'Hear', 'Feel', 'Social', 'Comm', 'Othref',
                    'Friends', 'Family', 'Humans', 'Time', 'Past', 'Present', 'Future', 'Space',
                    'Up', 'Down', 'Incl', 'Excl', 'Motion', 'Occup', 'School', 'Job', 'Achieve',
                    'Leisure', 'Home', 'Sports', 'TV', 'Music', 'Money', 'Metaph', 'Relig',
                    'Death',
                    'Physcal', 'Body', 'Sexual', 'Eating', 'Sleep', 'Groom', 'Swear']

        # Version 2007
        if version == 2007 and author == 'golbeck':
            return ['you', 'negate', 'article', 'negemo', 'sad', 'cogmech', 'cause', 'discrep',
                    'certain', 'hear', 'feel', 'social', 'family', 'humans', 'future', 'work',
                    'achieve', 'money', 'relig', 'death', 'bio', 'body', 'ingest', 'auxverb',
                    'quant', 'health', 'filler', 'Comma', 'Colon', 'QMark', 'Exclam', 'Parenth']

        # Version 2015 / Different author combination
        return [
            'pronoun', 'i', 'we', 'ppron', 'you', 'shehe', 'negate', 'assent', 'article',
            'preps', 'number', 'affect', 'posemo', 'negemo', 'anx', 'anger', 'sad', 'cogmech',
            'cause', 'insight', 'discrep', 'inhib', 'tentat', 'certain', 'percept', 'see',
            'hear', 'feel', 'social', 'friend', 'family', 'humans', 'time', 'past', 'present',
            'future', 'space', 'incl', 'excl', 'motion', 'work', 'achieve', 'leisure', 'home',
            'money', 'relig', 'death', 'bio', 'body', 'sexual', 'ingest', 'swear', 'auxverb',
            'quant', 'health', 'filler', 'Comma', 'Colon', 'QMark', 'Exclam', 'Parenth']

    def _get_liwc_df(self) -> pd.DataFrame:
        """
        Get the liwc scores as a dataframe.
        :return: LIWC scores in a pandas dataframe.
        """
        data = pd.read_csv(self.path)
        data.index = pd.to_numeric(data['Filename'].str.rstrip('.txt'))
        return data

    def _get_category_scores(self, data) -> pd.DataFrame:
        """
        Get the category scores from the LIWC CSV file and convert to a pandas dataframe.
        :return: LIWC category scores in a pandas dataframe.
        """
        return data[self._get_columns(self.version, self.author)]

    def apply_corrections(self, liwc_data, liwc_scores):
        """
        Apply corrections on the found scores applicable to the GitHub data.
        :param liwc_data: LIWC data containing the category scores.
        :param liwc_scores: Scores already found from the LIWC data.
        :return Updated dataframe with LIWC scores corrected for software engineering.
        """
        print('Applying corrections..')
        if self.author == 'yarkoni' and self.version == 2001:
            liwc_data_copy = liwc_data.copy()
            liwc_data_copy['Pronoun'] = np.log(liwc_data_copy['Pronoun'] + 1)
            liwc_data_copy['Social'] = liwc_data_copy['Social'] ** (1 / 2)
            liwc_scores['Agreeableness'] = liwc_data_copy.swifter.apply(
                self.get_agreeableness, axis=1, result_type='expand'
            )
            liwc_data_copy = liwc_data.copy()
            liwc_data_copy['Social'] = liwc_data_copy['Social'] ** (1 / 2)
            liwc_scores['Extraversion'] = liwc_data_copy.swifter.apply(
                self.get_extraversion, axis=1, result_type='expand'
            )
        elif self.author == 'golbeck' and self.version == 2007:
            liwc_data_copy = liwc_data.copy()
            liwc_data_copy['Exclam'] = np.log(liwc_data_copy['Exclam'] + 1)
            liwc_data_copy['hear'] = np.log(liwc_data_copy['hear'] + 1)
            liwc_scores['Neuroticism'] = liwc_data_copy.swifter.apply(
                self.get_neuroticism, axis=1, result_type='expand'
            )
            liwc_data_copy = liwc_data.copy()
            liwc_data_copy['you'] = liwc_data_copy['you'] ** (1 / 3)
            liwc_scores['Agreeableness'] = liwc_data_copy.swifter.apply(
                self.get_agreeableness, axis=1, result_type='expand'
            )
        return liwc_scores

    @staticmethod
    def calculate_sentiment(positive, negative):
        """
        Calculate the sentiment scores.
        :param positive: Number of words with positive sentiment.
        :param negative: Number of words with negative sentiment.
        :return: Average sentiment score multiplied by the correlation coefficient of Golbeck.
        """
        denominator = (positive - negative)
        numerator = (positive + negative)
        if numerator == 0:
            return 0
        return 0.268 * (denominator / numerator)

    def add_sentiment_scores(self, liwc_scores):
        """
        Add the sentment scores to the Openness score of Golbeck.
        :param liwc_scores: The LIWC scores found for Golbeck.
        :return: LIWC Scores with the Openness updated with the sentiment scores.
        """
        print("Applying sentiment scores.")
        hiv4 = ps.HIV4()
        for idx in tqdm(liwc_scores.index):
            try:
                with open(os.environ['LIWC_INPUT'] + str(idx) + '.txt', 'r') as f:
                    finput = f.readlines()[0]
                    tokens = hiv4.tokenize(finput)
                    sentiment_scores = hiv4.get_score(tokens)
                    liwc_scores.at[idx, 'Openness'] = liwc_scores.at[idx, 'Openness'] + \
                                                    self.calculate_sentiment(
                                                        sentiment_scores['Positive'],
                                                        sentiment_scores['Negative'])
            except FileNotFoundError:
                print('User {} does not exist in the {} {} results csv'.format(idx, self.author,
                                                                               self.version))
        return liwc_scores

    def get_liwc_scores(self) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """
        Get the LIWC personality scores calculated from the CSVs generated for each person.
        :return: Tuple of raw scores and normalized scores.
        """
        big_five_cols = ['Openness', 'Conscientiousness', 'Extraversion',
                         'Agreeableness', 'Neuroticism']

        liwc_data = self._get_category_scores(self._get_liwc_df())
        liwc_scores = pd.DataFrame(columns=big_five_cols)
        liwc_scores[big_five_cols] = liwc_data.swifter.apply(
            self.get_big_five_scores, axis=1, result_type='expand'
        )
        liwc_scores = self.apply_corrections(liwc_data, liwc_scores)
        if self.author == 'golbeck':
            liwc_scores = self.add_sentiment_scores(liwc_scores)

        # Min-max normalization
        liwc_scores_normalized = (liwc_scores - liwc_scores.min()) / (
                liwc_scores.max() - liwc_scores.min()) * 100
        return liwc_scores.round(15), liwc_scores_normalized.round(15)
