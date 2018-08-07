# R-Shiny-Dashboard
# accordian.js
$(document).on('shiny:bound', function() {
  var acc = document.getElementsByClassName('accordion');
  var i;

  for (i = 0; i < acc.length; i++) {
    acc[i].addEventListener('click', function() {
      this.classList.toggle('active');

      var panel = this.nextElementSibling;
      if (panel.style.display === 'block') {
        panel.style.display = 'none';
      } else {
        panel.style.display = 'block';
      }
    });
  }
});

# faq.md
## What data does this report contain?
This dashboard reports statistics related to quotes, sales and conversion ratio
for Commercial Lines Connect policies dating back to the initial launch of the
Connect platform. These data have been aggregated in a variety of useful ways to
help users gather essential information about our sales activity.

Each distinct aggregation is gathered in its own tab, which can be accessed from
the top portion of the sidebar on the lefthand side of the screen. These results
can then be filtered using the menus available in the bottom portion of the same
sidebar. Simply select the items that you wish to include in your results and
then press the "Apply Selected Filters" button to see your selections reflected
in the results. You will notice that the available filters are different for
each tab to ensure that your results remain relevant.

## Where do these data come from?
These data are sourced directly from the replicated PolicyCenter database. The
query logic used to acquire these data can be viewed within [this dashboard's
Phabricator repository page](https://phabricator.corp.stateauto.com/diffusion/QUOTE/browse/master/Source/quoteCloseGW.sql).

## How frequently are these data updated?
The data feeding this report are updated once per day at 4:00am. This helps to
prevent over-burdening the database with queries during the work day, and
provides users with their data on a 1-day lag. Once the data lake goes live we
will begin exploring data streaming options to provide these data in real time.

## What if I need additional detail?
Users who need additional details beyond the aggregations provided by this
report or who want to perform their own complex analyses can download the base
data that feed into this report at any time. Simply click the "Download Data"
dropdown in the top right corner of the dashboard and select "Download
Full Dataset" from that menu. This will allow you to download a CSV file of the
full, unfiltered dataset at the submission level.

## How do I report bugs?
If you believe that you've encountered an issue or inaccuracy with the report,
please [send me an email](mailto:thomas.cole@stateauto.com) with the words
"Quote Close Bug Report" in the subject line and a description and screenshot of
the issue in the body of the email. Please include any filters that you have
applied in this screenshot if at all possible. This will allow me to investigate
the problem and fix it in a timely fashion! Leaving these details out will
increase the amount of time it takes to address the issue.

## I'm a developer and I'd like to see the source code for this report.
That's not really a question now is it? Kidding aside, the source code for this
report is [available on Phabricator](https://phabricator.corp.stateauto.com/diffusion/QUOTE/)
for you to peruse. If you haven't registered for Phabricator yet, you can easily
create a new account using your LDAP credentials, AKA your Windows username and
password.

# style.css
.wide {
  width: 85%;
  margin-left: 7%;
  margin-right: 8%;
}

#timeStamp {
  text-align: center;
}

.skin-blue .header d.btn {
  color: #444;
}

.multicol .shiny-options-group {
  -webkit-column-count: 2; /* Chrome, Safari, Opera */
  -moz-column-count: 2;    /* Firefox */
  column-count: 2;
  -moz-column-fill: auto;
  -column-fill: auto;
}

.checkbox{
  margin-top: 0px !important;
  -webkit-margin-after: 0px !important;
}

.footer {
  position: absolute;
  bottom: 0;
  width: 100%;
  padding: 10px;
}

.accordion {
  cursor: pointer;
  padding: 18px;
  width: 100%;
  text-align: left;
  border: none;
  outline: none;
  transition: 0.4s;
  display: block;
}

.panel {
  background-color: #2c3b41;
  padding: 0 18px;
  display: none;
}

.sidebar-toggle {
  display: none;
}

.filter-menu {
  list-style: none;
  margin: 0;
  padding: 0;
}
