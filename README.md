# judge-prolog

Currently a PLUnit tester only

Can be tested with:

```bash
docker run --rm  -v $(realpath .):/home/runner/workdir dodona-prolog ./test/test.bash
```

To build use `docker buid -t dodona-prolog .`

Example output:

```json
{
  "groups": {
    "badgeCount": 5,
    "groups": [
      {
        "messages": [],
        "groups": [
          {
            "messages": [
              {
                "permission": "student",
                "format": "plain",
                "description": "OK"
              }
            ],
            "accepted": true
          }
        ],
        "accepted": true,
        "description": {
          "permission": "student",
          "format": "plain",
          "description": "realh"
        }
      },
      {
        "messages": [],
        "groups": [
          {
            "messages": [
              {
                "permission": "student",
                "format": "plain",
                "description": "OK"
              }
            ],
            "accepted": true
          }
        ],
        "accepted": true,
        "description": {
          "permission": "student",
          "format": "plain",
          "description": "realh1"
        }
      },
      {
        "messages": [],
        "groups": [
          {
            "messages": [
              {
                "permission": "student",
                "format": "markdown",
                "description": "**ERROR**: test nobrohug: assertion failed\nAssertion: brohug(tom,jan)"
              }
            ],
            "accepted": false
          },
          {
            "messages": [
              {
                "permission": "student",
                "format": "markdown",
                "description": "**ERROR**: test nobrohug: assertion failed\nAssertion: brohug(jan,fien)"
              }
            ],
            "accepted": false
          }
        ],
        "accepted": false,
        "description": {
          "permission": "student",
          "format": "plain",
          "description": "hugging"
        }
      },
      {
        "messages": [],
        "groups": [
          {
            "messages": [
              {
                "permission": "student",
                "format": "markdown",
                "description": "**ERROR**: test brohug: assertion raised \"catch/3: Undefined procedure: lol/2\"\nAssertion: lol(jan,tom)"
              }
            ],
            "accepted": false
          },
          {
            "messages": [
              {
                "permission": "student",
                "format": "markdown",
                "description": "**ERROR**: test brohug: assertion raised \"catch/3: Undefined procedure: lol/2\"\nAssertion: lol(tom,jan)"
              }
            ],
            "accepted": false
          },
          {
            "messages": [
              {
                "permission": "student",
                "format": "markdown",
                "description": "**ERROR**: test brohug: assertion raised \"catch/3: Undefined procedure: lol/2\"\nAssertion: lol(jan,fien)"
              }
            ],
            "accepted": false
          }
        ],
        "accepted": false,
        "description": {
          "permission": "student",
          "format": "plain",
          "description": "noname"
        }
      }
    ]
  },
  "accepted": false
}
```