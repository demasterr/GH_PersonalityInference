"""
Module containing all parsers for strings to pre-processed strings.
"""
import re
from nltk.stem.snowball import SnowballStemmer, PorterStemmer
from nltk.stem import WordNetLemmatizer


def get_word_count(text: str):
    """
    Get the word count of the parsed string.
    :return: Rough indication of the number of words in the string provided for analysis.
    """
    return len(re.findall(r'\S+', text))


class Parser:
    """
    Basic string parser.
    """
    COMP_IMAGES = re.compile(r'!\[[^\]]*\]\((.*?)(?=[")])(\".*\")?\)')
    COMP_CODE_BLOCKS_DOUBLE = re.compile(r'``\n.+\n?.*``|``.+\n?.*``|``.+\n?.+\n?``')
    COMP_CODE_BLOCKS_SINGLE = re.compile(r'`[^`]+`(?!`)')
    COMP_HASHTAGS = re.compile(r'#\d*(?=[^a-zA-Z0-9])|#')
    COMP_WHITESPACE = re.compile(r'[\n\t\r]')
    COMP_REFERENCES = re.compile(r'(?<![a-zA-Z])@(\w|\d)+')
    COMP_DOUBLE_WHITESPACE = re.compile(r'( {2,})+')
    COMP_NUMBERS = re.compile(r' [0-9]+[.,]*[0-9]*(?=[\W.])')
    COMP_SPACE_BEFORE_PUNCTUATION = re.compile(r' +(?=[,.!;?] )')
    COMP_IP_1 = re.compile(r'(([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|'
                           r'([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}'
                           r'(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4})'
                           r'{1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]'
                           r'{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]'
                           r'{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4})'
                           r'{0,4}%[0-9a-zA-Z]+|::(ffff(:0{1,4})?:)?((25[0-5]|(2[0-4]|'
                           r'1?[0-9])?[0-9])\.){3}(25[0-5]|(2[0-4]|1?[0-9])?[0-9])|([0-9a-fA-F]{1,4}'
                           r':){1,4}:((25[0-5]|(2[0-4]|1?[0-9])?[0-9])\.){3}(25[0-5]|(2[0-4]|1?[0-9])'
                           r'?[0-9]))')
    COMP_IP_2 = re.compile(r'^(25[0-5]|2[0-4][0-9]|[0-1]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[0-1]?'
                           r'[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[0-1]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]'
                           r'|[0-1]?[0-9][0-9]?)')
    COMP_URL = re.compile(r"(\w{3,}?://.)?(www\.)?[-a-zA-Z0-9@:%._+~#=]{2,256}[.:][a-zA-Z]{2,6}\b"
                          r"([-a-zA-Z0-9@:%_+.~#?&/=]*)")
    COMP_QUOTES = re.compile(r'\s*>.+')
    COMP_EMAILS = re.compile(r'[a-zA-Z0-9._-]+@([a-zA-Z0-9_-]+\.)+[a-zA-Z0-9_-]+')

    def __init__(self):
        self.parse_steps = [self.parse_lowercase, self.parse_code_blocks, self.parse_quotes,
                            self.parse_whitespace, self.parse_images, self.parse_hashtag,
                            self.parse_at_reference, self.parse_ip_address, self.parse_urls,
                            self.parse_numbers, self.parse_email,
                            self.parse_double_whitespace, self.parse_space_before_punctuation]

    def parse(self, content: str) -> (str, int):
        """
        Parser for message content.
        :param content: String content to be parsed.
        :return: Parsed string.
        """
        result = str(content)

        for func in self.parse_steps:
            result = func(result)
        return result, get_word_count(result)

    @staticmethod
    def parse_lowercase(content: str) -> str:
        """
        Remove the hashtags from the text.

        Example:
        TEsT MESSAge -> test message
        :param content: String that will be parsed.
        :return: String without the hashtags.
        """
        return content.lower()

    @staticmethod
    def parse_code_blocks(content: str) -> str:
        """
        Replace all code blocks with '*code*'.

        Examples:
        this `x=1` test -> this *code* test
        this ```x=1\nx=2``` test -> this *code* test
        :param content: String that will be parsed.
        :return: String with the code blocks replaced.
        """
        indices = [m.start() for m in (re.finditer('```', content))]

        # If there is an uneven number of ticks, the last one is unclosed. Remove this one first.
        if len(indices) % 2 == 1:
            content = content[:indices.pop()] + '*code*'

        while len(indices) > 0:
            second = indices.pop()
            first = indices.pop()
            content = content[:first] + '*code*' + content[second + 3:]

        content = Parser.COMP_CODE_BLOCKS_DOUBLE.sub('*code*', content)
        return Parser.COMP_CODE_BLOCKS_SINGLE.sub('*code*', content)

    @staticmethod
    def parse_images(content: str) -> str:
        """
        Remove the images from the text.

        Examples:
        #![](image.png) ->
        #![placeholder](https://remote.image) ->
        :param content: String that will be parsed.
        :return: String without the Markdown images.
        """
        return Parser.COMP_IMAGES.sub('', content)

    @staticmethod
    def parse_hashtag(content: str) -> str:
        """
        Remove the hashtags from the text.

        Examples:
        #release -> release
        #1 ->
        :param content: String that will be parsed.
        :return: String without the hashtags.
        """
        return Parser.COMP_HASHTAGS.sub('', content)

    @staticmethod
    def parse_whitespace(content: str) -> str:
        """
        Replace all possible whitespace (i.e., \n,\r,\t) with normal spaces.

        Examples:
        test\t\n\t\r -> test
        text\nand\ntest -> test and test
        :param content: String that will be parsed.
        :return: String without the non-space whitespace.
        """
        return Parser.COMP_WHITESPACE.sub(' ', content)

    @staticmethod
    def parse_at_reference(content: str) -> str:
        """
        Replace all at (@) references unless preceded by text.

        Examples:
        hello @username! -> hello !
        hello @2mato! -> hello !
        here my@mail -> here my@mail
        :param content: String that will be parsed.
        :return: String without the at references.
        """
        return Parser.COMP_REFERENCES.sub('', content)

    @staticmethod
    def parse_double_whitespace(content: str) -> str:
        """
        Replace all double spaces with single spaces.

        Example:
        test  test -> test test
        :param content: String that will be parsed.
        :return: String without the double spaces.
        """
        return Parser.COMP_DOUBLE_WHITESPACE.sub(' ', content).strip()

    @staticmethod
    def parse_numbers(content: str) -> str:
        """
        Replace all lone numbers.

        Examples:
        this 1 will be removed -> this will be removed
        1s l2 -> 1s l2
        :param content: String that will be parsed.
        :return: String without lone numbers.
        """
        return Parser.COMP_NUMBERS.sub('', content)

    @staticmethod
    def parse_space_before_punctuation(content: str) -> str:
        """
        Remove spaces before punctuations (. , ; !).

        Example:
        hello ! hello . hello , hello ; -> hello! hello. hello, hello;
        :param content: String that will be parsed.
        :return: String without lone numbers.
        """
        return Parser.COMP_SPACE_BEFORE_PUNCTUATION.sub('', content)

    @staticmethod
    def parse_ip_address(content: str) -> str:
        """
        Remove ip-addresses with possible port address.

        Example addresses:
        127.0.0.1, 127.0.0.1::3306, 2001:0db8:85a3:0000:1319:8a2e:0370:7344,
        2001:0db8:85a3:0000:1319:8a2e:0370:7344,
        2001:db8:::::1428:57ab, 2001::::
        :param content: String that will be parsed.
        :return: String without ip-addresses.
        """
        result = Parser.COMP_IP_1.sub('', content)
        return Parser.COMP_IP_2.sub('', result)

    @staticmethod
    def parse_urls(content: str) -> str:
        """
        Remove URLs with possible port address.

        Example addresses:
        http://video.google.co.uk:80/videoplay?docid=-12345&hl=en#00h02m30s
        foo://example.com:8042/over/there?name=ferret#nose
        www.cwi.nl:80
        http://example.com/cgi-bin/printenv.pl/pony?q=20%C001er&moar=kitties
        https://developer.mozilla.org/en/D
        http://www.ietf.org/rfc/rfc1945.txt
        http://www.w3.org/Addressing/URL/url-spec.txt
        :param content: String that will be parsed.
        :return: String without URLs.
        """
        return Parser.COMP_URL.sub('', content)

    @staticmethod
    def parse_quotes(content: str) -> str:
        """
        Remove quotes, as these are someone else's personality.

        Example quote:
        Yestday you said:
        > hello
        :param content: String that will be parsed.
        :return: String without quotes.
        """
        # Because the regex (it requires a preceding line break to make sure inline > are not
        # captured)is unable to capture quotes that happen to be in the first sentence,
        # this while-loop is used to capture all first quotes.
        result = content
        while result.startswith('>'):
            result = result.partition('\n')[2]
        return Parser.COMP_QUOTES.sub('', content)

    @staticmethod
    def parse_email(content: str) -> str:
        """
        Remove emails, as these are unlikely to capture personality in this context.

        Example email:
        user.name@mail.com
        :param content: String that will be parsed.
        :return: String without emails.
        """
        return Parser.COMP_EMAILS.sub('', content)


class StemParser:
    """
    Class used for stemming words in a text.
    """

    def __init__(self, stemmer='snowball', lemmatize=True):
        """
        Initialize stem parser.
        :param stemmer: Stemmer used.
            Either a string specifying the stemmer or an already initialized stemmer.
        :param lemmatize: True if 'e' at the end of a words should be kept (i.e. lemmatize).
        """
        if not isinstance(stemmer, str):
            self.stemmer = stemmer
        elif stemmer == 'snowball':
            self.stemmer = SnowballStemmer("english")
        elif stemmer == 'porter':
            self.stemmer = PorterStemmer()
        else:
            print("Unknown stemmer specified: " + str(stemmer))
        self.lemmatizer = WordNetLemmatizer() if lemmatize else None

    def stem(self, content: str) -> str:
        """
        Stem the words in 'content'.
        :param content: String to be stemmed.
        :return: Stemmed version of content.
        """
        tokenized = content.split()
        stemmed = []
        for element in tokenized:
            if self.lemmatizer:
                lemm = self.lemmatizer.lemmatize(element)
                element = lemm if lemm.endswith('e') else self.stemmer.stem(element)
            else:
                element = self.stemmer.stem(element)
            stemmed.append(element)
            stemmed.append(" ")
        if not stemmed:
            return ""
        stemmed.pop()
        return "".join(stemmed)
