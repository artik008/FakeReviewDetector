# Fake Review Detector

### Author: Artem Shitik

# Build

For building you need:
- stack (2.1.3) - https://www.haskell.org/platform/
- python (3.6.4) - https://www.python.org/downloads/

You need to check, that Python here: `/usr/local/bin/python3`

After installing:

`git clone git@github.com:artik008/FakeReviewDetector.git`

`cd FakeReviewDetector`

`stack biuld`

# Run

In file `teacher` - training sample for simple run.

Test urls you can add to file `test_url` (url of review in `otzovik.com`, like https://otzovik.com/review_3025967.html)

To run:
`stack exec FakeReviewDetector-exe -- teacher test_urls`