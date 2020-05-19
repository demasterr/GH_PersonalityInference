"""
Main module of the application.
"""
from analysis import Preprocessing, LIWC, PersonalityInsights, LIWCQuick
from config import Config
from os.path import exists
import shutil


def main():
    """
    Main function of the application.
    :return:
    """
    if not exists('config.json'):
        shutil.copy('default_config.json', 'config.json')
        print("Created 'config.json'. Please configure this file first.")
        return

    # Load configuration of the project.
    Config.load()

    config = Config.get_config()
    step = config['step']

    if step == 'PI':
        print("Personality Insights analysis started..")
        process = PersonalityInsights(config)
    elif step == 'LIWC':
        print("LIWC analysis started..")
        process = LIWC(config)
    elif step == 'Preprocess':
        print("Preprocessing started..")
        process = Preprocessing(config)
    elif step == 'QuickLIWC':
        print("Started LIWC process with pre-loaded LIWC files..")
        process = LIWCQuick(config)
    else:
        print("Unknown step specified in configuration.\nProgram terminated.")
        return
    process.execute()


if __name__ == "__main__":
    main()
