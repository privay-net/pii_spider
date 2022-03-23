[![MIT License](https://img.shields.io/apm/l/atomic-design-ui.svg?)](https://github.com/privay-net/pii_spider/blob/main/LICENSE)
![GitHub Release (latest by date including pre-releases)](https://img.shields.io/github/v/release/privay-net/pii_spider?display_name=tag&include_prereleases)
[![Issues](https://img.shields.io/github/issues-raw/privay-net/pii_spider.svg?maxAge=25000)](https://github.com/privay-net/pii_spider/issues) 
![main build](https://github.com/privay-net/pii_spider/actions/workflows/test.yml/badge.svg?branch=main)

# pii_spider
pii_spider is a tool that helps you find PII or sensitive data in a database.  The idea is that you don't know anything but a database connection and pii_spider will look in all your tables and tell what's there.

## Why Privacy Matters
OK, why should you care about privacy?  I'd argue there are two reasons privacy matters:
1. How do you get people to turst you?
We in technology are on the cusp of regularly upending people's lives.  If we as technologists are going to be trusted with people's lives and livlihoods then we have to demonstrate we're worthy of that.  So let's do so by managing people's data properly.
2. Privacy is safety
This is not a computing-specific point but people who talk about privacy being unecessary often assume there's no consequence of a loss of privacy.  There is.  In his book Data and Goliath Bruce Schneier says:
> “There's a strong physiological basis for privacy. Biologist Peter Watts makes the point that a desire for privacy is innate: mammals in particular don't respond well to surveillance. We consider it a physical threat, because animals in the natural world are surveilled by predators. Surveillance makes us feel like prey, just as it makes the surveillors act like predators.”

So having privacy means we're not acting like prey so we can feel safe.  We should be able to give that to people.

## Quickstart

    $ racket main.rkt --username db_user --database pii --password 'mypassword'
    $ open output/index.html

## More details
You'll find more details in the scribble documentation for this app.

## License

    pii_spider is licensed under the MIT License see LICENSE for more details.


