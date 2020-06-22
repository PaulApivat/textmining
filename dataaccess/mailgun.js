// access mailgun api in node.js environment
// var api_key = "API key"
// var domain = "API base URL"

const mailgun = require('mailgun-js');

var api_key = "API KEY"
var domain = "Domain"
var mg = mailgun({ apiKey: api_key, domain: domain })



var data = {
    from: 'Tester <me@samples.mailgun.org>',
    to: "paul@gmail.com",
    subject: 'Saku Testing',
    text: 'Testing some mailgun api'
};

mg.messages().send(data, function (error, body) {
    console.log(body);
})

console.log("is this working...")