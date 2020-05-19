"""
Test classes for the liwc module.
"""
import unittest

import numpy as np
from numpy.testing import assert_allclose

from liwc_methods import LIWCScores


class TestLIWCScores(unittest.TestCase):
    """
    Tests for the LIWCScore class.
    """

    def test_get_neuroticism(self):
        """
        Test if the right calculation is done for getting neuroticism.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([0.18])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_neuroticism(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_extraversion(self):
        """
        Test if the right calculation is done for getting extraversion.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([0.92])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_extraversion(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_openness(self):
        """
        Test if the right calculation is done for getting openness.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([-2.55])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_openness(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_acceptance(self):
        """
        Test if the right calculation is done for getting acceptance.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([1.08])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_agreeableness(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_conscientiousness(self):
        """
        Test if the right calculation is done for getting conscientiousness.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([-0.54])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_conscientiousness(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_big_five_scores(self):
        """
        Test if the right calculation is done for getting conscientiousness.
        """
        feed = np.array([[1] for _ in range(66)])
        expected = np.array([[-2.55], [-0.54], [0.92], [1.08], [0.18]])

        result = LIWCScores(signf=0.001, version=2001, author='yarkoni').get_big_five_scores(feed)
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_signficance_matrix_05(self):
        """
        Tets if you properly get the 0.05 precision matrix if you enter 0.05 as signf.
        """
        liwc = LIWCScores(signf=0.05, version=2001, author='yarkoni')
        expected = liwc.corr_05

        result = liwc._get_significance_matrix()
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_signficance_matrix_01(self):
        """
        Tets if you properly get the 0.01 precision matrix if you enter 0.01 as signf.
        """
        liwc = LIWCScores(signf=0.01, version=2001, author='yarkoni')
        expected = liwc.corr_01

        result = liwc._get_significance_matrix()
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_signficance_matrix_05_golbeck(self):
        """
        Tets if you properly get the 0.05 precision matrix if you enter 0.05 as signf and
        author Golbeck.
        """
        liwc = LIWCScores(signf=0.05, version=2007, author='golbeck')
        expected = liwc.corr_05_golbeck

        result = liwc._get_significance_matrix()
        assert_allclose(expected, result, rtol=1e-10, atol=0)

    def test_get_columns_2001(self):
        """
        Tets if all columns are returned of the LIWC 2001 Yarkoni version.
        """
        liwc = LIWCScores(signf=0.01, version=2001, author='yarkoni')
        expected = 66

        result = liwc._get_columns(2001)
        self.assertEqual(expected, len(result))

    def test_get_columns_2007(self):
        """
        Tets if all columns are returned of the LIWC 2007 Golbeck version.
        """
        liwc = LIWCScores(signf=0.01, version=2007, author='golbeck')
        expected = 61

        result = liwc._get_columns(2007)
        self.assertEqual(expected, len(result))
