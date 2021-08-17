# rwebq

## An R interface to the University of Washington [WebQ REST API](https://wiki.cac.washington.edu/display/aca/WebQ+REST+API) 

### Setup

Catalyst WebQ is available for members of University of Washington only. You must [generate a private key](https://catalyst.uw.edu/rest_user) using your UW login. 

Once you have done so, store your credentials using the `webq_config` function. This function stores your netid and API key as environment variables (`CATALYST_NETID`, `CATALYST_KEY`). 

```
webq_config(netid = 'sue', key = 'abcdefghijklmnopqrstuvwxyz123456789')
```

