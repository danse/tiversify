
# Tiversify

Is a twitter/x script to diversify the feed. Once called with:

    $ tiversify <handle>

It will analyse followed accounts and their posting rate, pointing to
those that fill a feed the most.

## Setup

Authentication uses OAuth 1.0a user context. Create an app in the
[X developer portal](https://developer.x.com/portal) and export its
credentials:

    export X_API_KEY=...          # consumer key
    export X_API_SECRET=...       # consumer secret
    export X_ACCESS_TOKEN=...     # access token
    export X_ACCESS_SECRET=...    # access token secret

## Usage

    $ tiversify <handle>
    Accounts with most messages:
    foo sent 30% of the messages
    ...
