var express = require('express'),
    _ = require('underscore'),
    fs = require('fs');

var router = express.Router();


/* grab db file */
var db = JSON.parse(fs.readFileSync('db.json'));

/* GET home page. */

router.get('/', function(req, res) {
  res.render('index');
});

router.get('/random-entries', function (req, res) {
  var entries = _.uniq(db.entries);
  res.json(_.sample(entries, 5));
});

router.get('/unauthorized', function(req, res) {
  res.sendStatus(401);
})

router.get('/not-found', function(req, res) {
  res.sendStatus(404);
})

module.exports = router;
