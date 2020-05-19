import mysql.connector
import pandas as pd
from sklearn import preprocessing


class ReadDB:

    def __init__(self, *, config, db_config):
        self.config = config
        self.db_config = db_config

    METHOD_COLUMNS = {
        'pi': {
            'pi_openness': 'PI_Openness',
            'pi_conscientiousness': 'PI_Conscientiousness',
            'pi_extraversion': 'PI_Extraversion',
            'pi_agreeableness': 'PI_Agreeableness',
            'pi_neuroticism': 'PI_Neuroticism'
        },
        'yarkoni': {
            'liwc_openness_yarkoni': 'LIWC_Openness_Yarkoni',
            'liwc_conscientiousness_yarkoni': 'LIWC_Conscientiousness_Yarkoni',
            'liwc_extraversion_yarkoni': 'LIWC_Extraversion_Yarkoni',
            'liwc_agreeableness_yarkoni': 'LIWC_Agreeableness_Yarkoni',
            'liwc_neuroticism_yarkoni': 'LIWC_Neuroticism_Yarkoni'
        },
        'golbeck': {
            'liwc_openness_golbeck': 'LIWC_Openness_Golbeck',
            'liwc_conscientiousness_golbeck': 'LIWC_Conscientiousness_Golbeck',
            'liwc_extraversion_golbeck': 'LIWC_Extraversion_Golbeck',
            'liwc_agreeableness_golbeck': 'LIWC_Agreeableness_Golbeck',
            'liwc_neuroticism_golbeck': 'LIWC_Neuroticism_Golbeck'
        }
    }

    PI_COLUMNS = ['PI_Openness', 'PI_Conscientiousness', 'PI_Extraversion', 'PI_Agreeableness',
                  'PI_Neuroticism']
    YARKONI_COLUMNS = ['LIWC_Openness_Yarkoni', 'LIWC_Conscientiousness_Yarkoni',
                       'LIWC_Extraversion_Yarkoni', 'LIWC_Agreeableness_Yarkoni',
                       'LIWC_Neuroticism_Yarkoni']
    GOLBECK_COLUMNS = ['LIWC_Openness_Golbeck', 'LIWC_Conscientiousness_Golbeck',
                       'LIWC_Extraversion_Golbeck', 'LIWC_Agreeableness_Golbeck',
                       'LIWC_Neuroticism_Golbeck']

    def _create_select(self, where, methods):
        select = ''
        for method in methods:
            for nth in range(0, 5):
                select += self._create_select_sub(method, nth) + ',\n'
        return select[:-2] + '\nFROM {}\n{}'.format(self.config['table'], where)

    def _create_select_sub(self, method, nth):
        trait = list(self.METHOD_COLUMNS[method].items())[nth]
        return '{}{{}} as {}'.format(*trait)

    def read_db(self, where, methods=('pi', 'yarkoni', 'golbeck'), raw=True, decimals=1,
                user_id=True, scaling=True):
        suffix = '_raw' if raw else ''
        where = 'WHERE ' + where if where else ''
        select = 'SELECT id as user_id,\n' if user_id else 'SELECT \n'
        query = select + self._create_select(where, methods).format(*(suffix,) * 15)
        results = self.execute_query(query)
        cols = [column for sublist in
                [list(self.METHOD_COLUMNS[method].values()) for method in methods] for column in
                sublist]
        if user_id:
            cols.insert(0, 'user_id')
        dataframe = pd.DataFrame(results, columns=cols)
        dataframe = dataframe.apply(pd.to_numeric)
        if user_id:
            cols.remove('user_id')
        if scaling:
            scaler = preprocessing.MinMaxScaler(feature_range=(0, 100))
            dataframe[cols] = scaler.fit_transform(dataframe[cols])
        return dataframe.round(decimals=decimals), cols

    def execute_query(self, query):
        cnx = mysql.connector.connect(**self.db_config)
        cursor = cnx.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cnx.close()
        return results
