"""
Test classes for the parsing module.
"""
import unittest

from parameterized import parameterized

from parsing import Parser, StemParser


class TestParser(unittest.TestCase):
    """
    Tests for the Parser class.
    """

    def test_parse_lowercase(self):
        """
        Test if the content is properly converted to all lowercase.
        """
        feed = "ThIs iS a TEST"
        expected = "this is a test"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_replace_enter_space(self):
        """
        Test if enters are properly replaced with spaces.
        """
        feed = "this is\na test\n"
        expected = "this is a test "

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_replace_enter_double_space(self):
        """
        Test if enters are properly replaced with spaces, unless there already is a space.
        """
        feed = "this is \n a test \n"
        expected = "this is a test "

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_strip_double_whitespace(self):
        """
        Test if double whitespace is removed properly.
        """
        feed = "hello  world"
        expected = "hello world"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_at_username_removed(self):
        """
        Test if @username mentions are removed properly.
        """
        # TODO: check if this should be removed or be replaced with '(mention)'.
        feed = "hello @username!"
        expected = "hello !"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_qoute_removed(self):
        """
        Test if quotes are properly removed.
        """
        # TODO: should quotes be redirected? Or be replaced with '(quote)'?
        feed = "hello \n> this is a quote!\nworld"
        expected = "hello world"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_url_removed_http(self):
        """
        Test if URLs are properly removed if 'http' format.
        """
        feed = "this is a url http://google.com"
        expected = "this is a url "

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_url_removed_https(self):
        """
        Test if URLs are properly removed if 'https' format.
        """
        feed = "this is a url (https://google.com)"
        expected = "this is a url ()"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_url_removed_www(self):
        """
        Test if URLs are properly removed if 'www' format.
        """
        feed = "this is a url (www.google.com)"
        expected = "this is a url ()"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_url_removed_ftp(self):
        """
        Test if URLs are properly removed if 'ftp' format.
        """
        feed = "this is an ftp url ftp://user:password@host:port/path"
        expected = "this is an ftp url "

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_number_alone(self):
        """
        Test if numbers are removed only when alone.
        """
        feed = "this is a number 1 here"
        expected = "this is a number here"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_number_hashtag(self):
        """
        Test if numbers are removed when combined with hashtag.
        """
        feed = "this is an issue reference #1 here"
        expected = "this is an issue reference here"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_email(self):
        """
        Test if emails are removed properly when combined with other parsing steps.
        """
        feed = "this is an email address user.name@mail.com right here!"
        expected = "this is an email address right here!"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_number_combined(self):
        """
        Test if numbers are kept if they are part of a word.
        """
        # TODO: Should later be replaced with abbrevation mapping?
        feed = "this is 4every1 m8."
        expected = "this is 4every1 m8."

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_hashtag_word(self):
        """
        Test if hashtag content is kept and only hashtag is removed.
        """
        feed = "it is #release time!"
        expected = "it is release time!"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_code_inline(self):
        """
        Test if inline code is replaced with '*code*'.
        """
        feed = "here is some inline `var x = 'true'` code!"
        expected = "here is some inline *code* code!"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_parse_code_block(self):
        """
        Test if code blocks are replaced with '*code*'.
        """
        feed = """here is some
        ```
        var y = 1;
        var x = y;
        x = x + 1;
        ```
        block!"""
        expected = "here is some *code* block!"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_hashtag_idv(self):
        """
        Test if the hashtag is properly applied to many edge cases, individually.
        """
        feed = "#notcaptured, issue #11.#release #2you and #11 removed and #1111, #test, #@! #111."
        expected = "notcaptured, issue .release 2you and  removed and , test, @! ."

        result = Parser().parse_hashtag(feed)
        self.assertEqual(expected, result)

    def test_at_idv(self):
        """
        Test if at (@) is properly applied to many edge cases, individually.
        """
        feed = "#@! #1..111. @hans how you doin'? My name is @username " \
               "and @1love is with us! you@mail"
        expected = "#@! #1..111.  how you doin'? My name is  and  is with us! you@mail"

        result = Parser().parse_at_reference(feed)
        self.assertEqual(expected, result)

    def test_double_whitespace_idv(self):
        """
        Test if multi-whitespace is properly replaced to single whitespace, individually.
        """
        feed = "this     contains  many    spaces that     should be   replaced !"
        expected = "this contains many spaces that should be replaced !"

        result = Parser().parse_double_whitespace(feed)
        self.assertEqual(expected, result)

    def test_parse_at_username_removed_idv(self):
        """
        Test if @username mentions are removed properly, individually.
        """
        feed = "hello @username!"
        expected = "hello !"

        result = Parser().parse_at_reference(feed)
        self.assertEqual(expected, result)

    def test_parse_code_inline_idv(self):
        """
        Test if inline code is replaced properly, individually.
        """
        feed = "this `x=1; y=2` is replaced"
        expected = "this *code* is replaced"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_idv(self):
        """
        Test if inline code blocks are replaced properly, individually.
        """
        feed = "this ```x=1; y=2``` is replaced"
        expected = "this *code* is replaced"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_multiline_idv(self):
        """
        Test if multiline code blocks are replaced properly, individually.
        """
        feed = """this ```
        x=1;
        y=2
        ``` is replaced"""
        expected = "this *code* is replaced"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_ticks_inside(self):
        """
        Test if a nested code block is not interfering with the outer triple tick code block.
        """
        feed = """
        I honestly don't see the need of a new special method just to accomodate ActiveRecord needs. Moreover, the check `input =~ /[0-9]/` is very opinionable, other users might need a different validation match and we will end up trying to match everyone's needs.

What's wrong with the following custom validation?

```ruby
composed_of :price,
  :class_name => "Money",
  :mapping => [%w(price_cents cents), %w(currency currency_as_string)],
  :constructor => Proc.new { |cents, currency| Money.new(cents || 0, currency || Money.default_currency) },
  :converter => Proc.new { |value| 
    if value.respond_to?(:to_money)
      if input =~ /[0-9]/
        value.to_money
      else
        raise ArgumentError, "`input' should contain a numeric character"
      end
    else
      raise ArgumentError, "Can't convert #{value.class} to Money"
    end
  }
```

PS. In case the patch will pass the approval, please note that tests are requires in order to merge the pull request.

BTW, thanks for taking the time to contribute!"""
        expected = """
        I honestly don't see the need of a new special method just to accomodate ActiveRecord needs. Moreover, the check *code* is very opinionable, other users might need a different validation match and we will end up trying to match everyone's needs.

What's wrong with the following custom validation?

*code*

PS. In case the patch will pass the approval, please note that tests are requires in order to merge the pull request.

BTW, thanks for taking the time to contribute!"""
        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_ignore_idv(self):
        """
        Test a single tick, that is not a code block, is ignored.
        """
        feed = "hello `not a code block"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(feed, result)

    def test_parse_code_block_double(self):
        """
        Test if double tick code blocks multi line work.
        """
        feed = "hello ``test\ntest`` world"
        expected = "hello *code* world"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_double_enter(self):
        """
        Test if double tick code blocks with enter inbetween is ignored.
        """
        feed = "hello ``test\n\ntest`` world"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(feed, result)

    def test_parse_code_block_no_end(self):
        """
        Test if unclosed code blocks are still captured.
        """
        feed = "hello ```\n\n\n\n\nworld"
        expected = "hello *code*"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_code_block_double_no_enter(self):
        """
        Test if double tick code blocks with enter inbetween is ignored.
        """
        feed = "hello ``test test`` world"
        expected = "hello *code* world"

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_parse_space_before_punctuation_idv(self):
        """
        Test if spaces are removed properly before punctuation, individually.
        """
        feed = "hello ! hello . hello , hello ; "
        expected = "hello! hello. hello, hello; "

        result = Parser().parse_space_before_punctuation(feed)
        self.assertEqual(expected, result)

    def test_parse_ip_address_idv(self):
        """
        Test if ip-addresses are removed properly (if actually valid), individually.
        """
        feed = """127.0.0.1, 2001:0db8:85a3:0000:1319:8a2e:0370:7344,
        2001:0db8:85a3:0000:1319:8a2e:0370:7344, 2001::::"""
        expected = """, ,
        , """

        result = Parser().parse_ip_address(feed)
        self.assertEqual(expected, result)

    def test_parse_ip_address_idv2(self):
        """
        Test if ip-addresses are removed properly, individually.
        """
        feed = """127.0.0.1"""
        expected = """"""

        result = Parser().parse_ip_address(feed)
        self.assertEqual(expected, result)

    def test_parse_number_combined_idv(self):
        """
        Test if numbers are kept if they are part of a word with only number pre-processing.
        """
        feed = "this is 4every1 m8."
        expected = "this is 4every1 m8."

        result = Parser().parse_numbers(feed)
        self.assertEqual(expected, result)

    @parameterized.expand([
        ["http://video.google.co.uk:80/videoplay?docid=-72469276128318230&hl=en#00h02m30s", ""],
        ["foo://example.com:8042/over/there?name=ferret#nose", ""],
        ["www.cwi.nl:80", ""],
        ["http://example.com/cgi-bin/printenv.pl/pony?q=20%C001er&moar=kitties", ""],
        ["https://developer.mozilla.org/en/D", ""],
        ["http://www.ietf.org/rfc/rfc1945.txt", ""],
        ["http://www.w3.org/Addressing/URL/url-spec.txt", ""],
        ["ftp://user:password@host:port/path", ""],
        ["http not a url!", "http not a url!"],
    ])
    def test_parse_url_idv(self, inp, exp):
        """
        Test if URL are all removed.
        """
        result = Parser().parse_urls(inp)
        self.assertEqual(exp, result)

    def test_parse_ip_ignore_url(self):
        """
        Test if URLs are ignored by the ip-address parsing.
        """
        feed = "ignore this url http://google.com"
        expected = "ignore this url http://google.com"

        result = Parser().parse_ip_address(feed)
        self.assertEqual(expected, result)

    @parameterized.expand([
        ["0", "2001:db8:3333:4444:5555:6666:7777:8888", ""],
        ["1", "2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF", ""],
        ["2", "::", ""],
        ["3", "2001:db8::", ""],
        ["4", "::1234:5678", ""],
        ["6", "2001:0db8:0001:0000:0000:0ab9:C0A8:0102", ""]
    ])
    def test_parse_ip_idv(self, name, inp, exp):
        """
        Test if ips are removed properly.
        """
        result = Parser().parse_ip_address(inp)
        self.assertEqual(exp, result)

    def test_parse_qoute_removed_idv(self):
        """
        Test if quotes are properly removed, individually.
        """
        feed = "hello\n  > this is a quote!\nworld"
        expected = "hello\nworld"

        result = Parser().parse_quotes(feed)
        self.assertEqual(expected, result)

    def test_parse_qoute_first(self):
        """
        Test if quotes are properly removed if it is the very first element.
        """
        feed = ">this is a quote!\nworld"
        expected = "\nworld"

        result = Parser().parse_quotes(feed)
        self.assertEqual(expected, result)

    def test_parse_code_edge_cases(self):
        """
        Test if code blocks are even in valid edge cases captured.
        """
        feed = """
        This is a `test`
        
        just as this `test
        test
        `
        
        ```
        test
        """
        expected = """
        This is a *code*
        
        just as this *code*
        
        *code*"""

        result = Parser().parse_code_blocks(feed)
        self.assertEqual(expected, result)

    def test_plus(self):
        """
        Test if +1 is kept.
        """
        feed = "+1"
        expected = "+1"

        result = Parser().parse(feed)[0]
        self.assertEqual(expected, result)

    def test_big_number(self):
        """
        Test if big numbers like 10,000 are captured.
        """
        feed = "i bought 10,000 cookies"
        expected = "i bought cookies"

        result = Parser().parse_numbers(feed)
        self.assertEqual(expected, result)

    def test_decimal_number(self):
        """
        Test if decimal numbers are captured.
        """
        feed = "pi is approximately 3.14159265359."
        expected = "pi is approximately."

        result = Parser().parse_numbers(feed)
        self.assertEqual(expected, result)

    def test_image_parsing(self):
        """
        Test if single image is captured without alt-text.
        """
        feed = "here is my image: ![](image.png)"
        expected = "here is my image: "

        result = Parser().parse_images(feed)
        self.assertEqual(expected, result)

    def test_image_parsing_multiple(self):
        """
        Test if multiple image are captured.
        """
        feed = "here is my image: ![](image.png) and another one ![](something) here!"
        expected = "here is my image:  and another one  here!"

        result = Parser().parse_images(feed)
        self.assertEqual(expected, result)

    def test_image_parsing_alt_text(self):
        """
        Test if single image is captured with alt-text.
        """
        feed = "here is my image: ![alt-text](image.png) and another one ![test](something) here!"
        expected = "here is my image:  and another one  here!"

        result = Parser().parse_images(feed)
        self.assertEqual(expected, result)

    def test_image_parsing_alternatives(self):
        """
        Test if some cases do not work.
        """
        feed = "do not remove! [this] (here) [![](image.png)] [](notimage.png)"
        expected = "do not remove! [this] (here) [] [](notimage.png)"

        result = Parser().parse_images(feed)
        self.assertEqual(expected, result)


class TestStemParser(unittest.TestCase):
    """
    Tests for the StemParser class.
    """

    def test_parse_word_stemming(self):
        """
        Test if the word stemming is properly mapping values to a its root.
        """
        feed = "swim swam swimming"
        expected = "swim swam swim"

        result = StemParser().stem(feed)
        self.assertEqual(expected, result)

    def test_parse_word_stemming_empty(self):
        """
        Test if the word stemming does not fail when the string is empty.
        """
        feed = ""
        expected = ""

        result = StemParser().stem(feed)
        self.assertEqual(expected, result)


if __name__ == '__main__':
    unittest.main()
