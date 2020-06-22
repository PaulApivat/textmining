// access mailgun api in node.js environment
// var api_key = "API key"
// var domain = "API base URL"

var mailgun = require('mailgun-js');

var api_key = "API KEY"
var domain = "DOMAIN"
var mg = mailgun({ apiKey: api_key, domain: domain })

var data = {
    from: 'Excited Tester <me@samples.mailgun.org>',
    to: 'some.user@hotmail.com',
    subject: 'Saku Testing',
    text: 'Testing some mailgun api'
};

mg.messages().send(data, function (error, body) {
    console.log(body);
})

console.log("is this working...")