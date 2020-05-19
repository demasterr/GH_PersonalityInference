import matplotlib.pyplot as plt

from stats.db import ReadDB
from sklearn.cluster import KMeans
from yellowbrick.cluster import KElbowVisualizer
from collections import Counter
import seaborn as sns
import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error
from math import sqrt


class VisualizationException(Exception):
    pass


class Visualizer:

    TRAIT_TYPES = ['Openness', 'Conscientiousness', 'Extraversion', 'Agreeableness', 'Neuroticism']

    @staticmethod
    def sort_df(dataframe, columns, sort_on, ascending=True):
        """
        Sort the dataframe based on the column.
        :param dataframe: Dataframe that will be sorted.
        :param columns: Columns selected for the graph.
        :param sort_on: Column to be sorted on.
        :param ascending: True if sort in ascending order (descending otherwise).
        :return: Sorted dataframe.
        """
        sorted_dataframe = dataframe[columns].sort_values(sort_on, ascending=ascending)
        sorted_dataframe.index = range(len(sorted_dataframe))
        return sorted_dataframe

    @staticmethod
    def plot_sorted(dataframe, method_columns):
        """
        Plot the graphs sorted on one of the personality traits for all personality traits.
        :param dataframe: Dataframe that will be drawn for the graph.
        :param method_columns: Trait columns for each method.
        """
        fig, axs = plt.subplots(5, 3, figsize=(15, 15))
        fig.subplots_adjust(hspace=0.3)
        for num in range(0, 5):
            cols = []
            for method in list(method_columns.keys()):
                cols.append(list(method_columns[method].items())[num][1])
            col_n = 0
            for col in cols:
                sorted_df = Visualizer.sort_df(dataframe, columns=cols, sort_on=col)
                sorted_df.index = range(len(sorted_df))
                axs[num, col_n].plot(sorted_df)
                axs[num, col_n].set_title(col)
                col_n += 1

    @staticmethod
    def boxplt_all(dataframe, columns):
        """
        Draw all boxplots possible in the dataframe (based on the columns in the dataframe).
        :param dataframe: Dataframe for which all boxplots will be drawn.
        :param columns: Columns for which the boxplot will be drawn.
        """
        fig, axs = plt.subplots(5, int(len(columns) / 5), figsize=(15, 15))
        fig.subplots_adjust(hspace=0.3)
        for method_num in range(0, int(len(columns) / 5)):
            for trait_num in range(0, 5):
                idx = method_num * 5 + trait_num
                Visualizer.boxplt(dataframe, columns[idx], axs[trait_num, method_num])

    @staticmethod
    def boxplt(data, col, axis):
        """
        Draw a matplotlib boxplt.
        :param data: Dataframe that will be drawn.
        :param col: Column used for the boxplot.
        :param axis: Axis on which the boxplot will be drawn.
        """
        axis.boxplot(data[col])
        axis.set_title(col)

    @staticmethod
    def compare_all(df_a, df_b, func, *, title=None):
        """
        Compare method-wise each personality trait for dataframes a and b using visualizatoin
        function func.
        :param df_a: First dataframe to be compared to b.
        :param df_b: Second dataframe to be compared to a.
        :param func: Function used for visualization (e.g., boxplt).
        :param title: Title for the subfigures.
        """
        cols = list(df_a.columns)
        if cols != list(df_b.columns):
            raise VisualizationException(
                'Comparison of unequally shaped dataframe is not possible\n'
                '[a: {}\nb: {}]'.format(cols, list(df_b.columns)))
        for col_set in (ReadDB.PI_COLUMNS, ReadDB.YARKONI_COLUMNS, ReadDB.GOLBECK_COLUMNS):
            if set(col_set).issubset(cols):
                Visualizer.compare(df_a[col_set], df_b[col_set], func, title=title)

    @staticmethod
    def compare(df_a, df_b, func, *, title=None):
        """
        Compare two dataframe df_a and df_b to each other and visualize using func.
        :param df_a: Dataframe a to be compared to dataframe b.
        :param df_b: Dataframe b to be compared to dataframe a.
        :param func: Function used for visualization (e.g., boxplt).
        :param title: Title for the subfigures.
        """
        cols = list(df_a.columns)
        if cols != list(df_b.columns):
            raise VisualizationException(
                'Comparison of unequally shaped dataframe is not possible\n'
                '[a: {}\nb: {}]'.format(cols, list(df_b.columns)))

        fig, axs = plt.subplots(len(cols), 2, figsize=(15, 15))
        if title:
            fig.suptitle(title)
        for trait_num, trait in enumerate(cols):
            func(df_a, trait, axs[trait_num, 0])
            func(df_b, trait, axs[trait_num, 1])

    @staticmethod
    def compare_line(df_a, df_b, methods=('pi', 'yarkoni', 'golbeck')):
        """
        Compare the line graphs of two dataframes A and B based on specified methods.
        :param df_a: Dataframe A to be compared to dataframe B.
        :param df_b: Dataframe B to be compared to dataframe A.
        :param methods: Methods (scores) for which a line will be drawn.
        """
        fig, axs = plt.subplots(5, 2, figsize=(15, 15))
        fig.subplots_adjust(hspace=0.5)
        for idx, trait_type in enumerate(Visualizer.TRAIT_TYPES):
            for method in methods:
                column = list(ReadDB.METHOD_COLUMNS[method].values())[idx]
                sorted_df = Visualizer.sort_df(df_a, [column], sort_on=column, ascending=True)
                axs[idx, 0].plot(sorted_df, label=method)
                sorted_df = Visualizer.sort_df(df_b, [column], sort_on=column, ascending=True)
                axs[idx, 1].plot(sorted_df, label=method)
            axs[idx, 0].set_title(trait_type + ' A')
            axs[idx, 0].legend()
            axs[idx, 1].set_title(trait_type + ' B')
            axs[idx, 1].legend()

    @staticmethod
    def find_optimal_k(dataframe, clusters=(1, 15)):
        """
        Find optimal cluster value k with elbow curve.
        :param dataframe: Dataframe to be clustered using KMeans.
        :param clusters: Number of clusters to be tried. Default 1 to 15 clusters.
        """
        model = KMeans()
        visualizer = KElbowVisualizer(model, k=clusters)
        visualizer.fit(dataframe)
        visualizer.show()

    @staticmethod
    def cluster_and_count(dataframe, column, *, n_clusters=10):
        """
        Cluster the dataframe and count the values in each cluster.
        :param dataframe: Dataframe on which the clustering is applied
        :param column: Column for which we apply the KMeans.
        :param n_clusters: Number of clusters for the KMeans.
        :return: Counts for each cluster and the KMeans model.
        """
        data = dataframe[[column]].to_numpy()
        kmeans = KMeans(
            n_clusters=n_clusters, random_state=0, n_init=1,
            init=np.array(np.linspace(0, 100, n_clusters)).reshape(-1, 1)
        ).fit(data.reshape(-1, 1))
        counter = Counter(kmeans.labels_)
        values = [counter.get(idx) for idx in range(0, len(counter))]
        return values, kmeans

    @staticmethod
    def cluster_plot(dataframe, column, axis=None, *, n_clusters=10, values=None):
        """
        Draw the cluster plot as a bar chart.
        :param dataframe: Dataframe on which we apply the clustering.
        :param column: Column on which the clustering is applied.
        :param axis: Axis on which the barchart is drawn.
        :param n_clusters: Number of clusters used for clustering.
        :param values: Optional values if cluster_and_count is already done.
        :return: Counts of items in each cluster.
        """
        if not values:
            values, _ = Visualizer.cluster_and_count(dataframe, column, n_clusters=n_clusters)
        # Draw clustering plot
        Visualizer._draw_cluster_plot(dataframe, values, axis=axis)
        return values

    @staticmethod
    def show_clustering(dataframe, column, *, n_clusters=10, show_sample_size=5000):
        """
        Show the clustering when applied to the dataframe in a scatter plot.
        :param column: Column on which the clustering is applied.
        :param n_clusters: Number of clusters applied to the dataframe.
        :param show_sample_size: No. of samples randomly taken to show on the scatter plot.
        """
        _, kmeans = Visualizer.cluster_and_count(dataframe, column, n_clusters=n_clusters)
        sorted_cluster = pd.DataFrame(dataframe[column])
        sorted_cluster['cluster'] = kmeans.labels_
        sorted_cluster = sorted_cluster.sample(show_sample_size)
        sns.scatterplot(x=list(sorted_cluster.index),
                        y=sorted_cluster[column],
                        hue=sorted_cluster['cluster']).set(title='Clustering overview')
        plt.show()

    @staticmethod
    def _draw_cluster_plot(dataframe, values, *, axis):
        """
        Draw a bar chart showing all clusters and their respective number of items in each cluster.
        :param dataframe: Dataframe on which the clustering is applied.
        :param values: Counts of items in each cluster.
        :param axis: Axis on which the bar chart will be drawn.
        """
        title = dataframe.name if hasattr(dataframe, 'name') else ''
        if not axis:
            with sns.axes_style('whitegrid'):
                _, axis = plt.subplots(1, 1)
        sns.barplot(x=list(range(0, len(values))), y=values, ax=axis).set(title=title,
                                                                          xlabel='Clusters',
                                                                          ylabel='Cluster size')

    @staticmethod
    def compare_cluster_plot(df_a, df_b, column, *, n_clusters, show=True):
        """
        Compare two clusterings of two different dataframes and show the plots if desired.
        :param df_a: First dataframe to be clustered.
        :param df_b: Second dataframe to be clustered.
        :param column: Column on which will be compared.
        :param n_clusters: Number of clusters chosen.
        :param show: If True, show both clustering bar charts.
        :return: Item counts for each cluster in both clusterings as tuple.
        """
        with sns.axes_style('darkgrid'):
            values_a, _ = Visualizer.cluster_and_count(df_a, column, n_clusters=n_clusters)
            values_b, _ = Visualizer.cluster_and_count(df_b, column, n_clusters=n_clusters)
            if show:
                f, axes = plt.subplots(1, 2, figsize=(16, 6))
                max_y = max(max(values_a), max(values_b))
                axes[0].set_ylim(0, max_y)
                axes[1].set_ylim(0, max_y)
                Visualizer.cluster_plot(df_a, column, axis=axes[0], n_clusters=n_clusters,
                                        values=values_a)
                Visualizer.cluster_plot(df_b, column, axis=axes[1], n_clusters=n_clusters,
                                        values=values_b)
            return values_a, values_b

    @staticmethod
    def absolute_difference(list_a, list_b):
        """
        Get the absolute difference between two lists A and B.
        :param list_a: List A to be compared to list B.
        :param list_b: List B to be compared to list A.
        :return: The absolute difference between lists A and B.
        """
        if len(list_a) != len(list_b):
            raise VisualizationException(
                "Could not do an absolute comparison on lists with different shapes [{} vs {}]".format(
                    len(list_a), len(list_b)))
        diff = [abs(list_a[idx] - list_b[idx]) for idx in range(0, len(list_a))]
        return diff, sum(diff)

    @staticmethod
    def rmse(list_a, list_b):
        """
        Get the RMSE (Root Mean Squared Error) between lists A and B.
        :param list_a: List A to be compared to list B.
        :param list_b: List B to be compared to list A.
        :return: RMSE between lists A and B.
        """
        return sqrt(mean_squared_error(list_a, list_b))
