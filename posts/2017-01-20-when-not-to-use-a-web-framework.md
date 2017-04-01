---
title: When Not to Use a Web Framework
postUrl: /posts/when-not-to-use-a-web-framework/
imageUrl: "/images/frameworks-large.jpg"
description: "
<p>Sometimes, it's actually best to ditch your mainstream web framework when
starting new development or refactoring an existing project. If you try to make
an opinionated tool do something that it wasn't designed to do, then prepare
for a bloated budget and missed deadlines. It's important to understand what
mainstream web frameworks are good at, and where they fall short.</p>
"
featuredOrder: 3
relatedPosts:
    - "2017-01-15-cms-or-custom-software.md"
    - "2017-03-14-just-build-it-in-django-or-rails.md"
---


All rules have exceptions. There are definitely situations where web frameworks
such as  Rails and Django may not be the first choices for use in developing
custom web software.

Here are some known categories of exceptions, and several examples per category:

## Low-latency and/or high-throughput

Think about how often someone will access your service. How many clicks will
they make in an hour, and how soon do they expect a response? Examples of good
candidate applications for web frameworks are social networks, p2p
marketplaces, and ecommerce stores. All have pretty lax requirements on these
metrics per user.

In order to make it simple and fast to model your business processes, web
frameworks add several layers of abstraction that, as a side effect, slow down
the processing of user requests. In most applications, this isn't a problem
and you can just throw more servers and resources to achieve a speedy request
handling. However, some businesses processes with huge performance demands per
user are just not well-suited to web frameworks.

### Online Games

Have you ever played or watched someone play a first-person shooter such as
Halo, Counter-Strike, Overwatch? What about other online multiplayer games like
World of Warcraft, Minecraft, or Farmville? The common thread for these online
games is that as soon as someone has to wait longer than 30-250 milliseconds to
get a response to their click, keypress, or tap, they start having a horrible
experience. If you're ambitious enough to tackle game development, then you're
probably already knowledgeable enough to know that most web frameworks aren't
for you. The software architecture for networked games is vastly different than
those of typical enterprise applications, and they are notoriously difficult to
implement and scale. The exceptions to this may be page-based games such as
Neopets.

### Messaging Platforms

Whatsapp is a messaging platform that was acquired by Facebook for $19 billion.
Its core focus was to deliver an amazing chat experience to billions of users.
It was not built on Django or Rails for very good reasons: it would not have
been a good return on investment as many of the components would have been
pretty useless to the core strategy of fast, reliable, and high-volume message
passing. In 2014, it processed 50 billion messages every day. By leveraging
Erlang, the <i>fifty</i> engineers at Whatsapp were able to handle over 2.5
million connections per server, resulting in massive cost savings in employees
and equipment.

[<i class="linkify icon"></i>The WhatsApp Architecture Facebook Bought For $19
Billion](http://highscalability.com/blog/2014/2/26/the-whatsapp-architecture-facebook-bought-for-19-billion.html)

### Video Livestreaming
Video Livestreaming services like Twitch and Periscope that support
thousands of concurrent users use video processing backends such as the
Red5, Wowza Media Server, or custom and proprietary implementations.
This [Amazon AWS page](https://aws.amazon.com/cloudfront/streaming/)
lists some popular backends for video processing, encoding, and
distribution. Django and Rails were not meant for high data throughput
media streams, and therefore don't come stock with any facilities to
handle them. This is a different enough problem where it's best to go
with a solution that knows about media streaming. However, the websites
and mobile apps for these services can definitely be backed by a Django
or Rails instance to handle logins, page rendering, and more; just not
the streaming itself. It's possible to have these web frameworks hand
off to specialized media processing engines through an interface.

[<i class="linkify icon"></i>What Technology Powers Periscope's Live Video Stream?](https://yalantis.com/blog/what-technology-powers-periscope/)

[<i class="linkify icon"></i>Twitch Engineering: An Introduction and Overview](https://blog.twitch.tv/twitch-engineering-an-introduction-and-overview-a23917b71a25#.kb7h2bt7y)

### Web/IoT Analytics Collection

Web analytic companies like Google Analytics, Chartbeat, or Heap Analytics are
technically challenging to implement because every single click for any of your
tracked websites creates a data entry that must be processed. For a portfolio
of busy websites, this can easily result in hundreds of thousands to millions
of requests per second that must be handled efficiently as not to break the
service. These screaming speeds usually demand pretty custom distributed
systems based on technologies such as [Kafka](https://kafka.apache.org/) and
[Cassandra](https://cassandra.apache.org/).

[<i class="linkify icon"></i>Chartbeat Backend Engineering
Blog](http://blog.chartbeat.com/tag/backend-engineering/)


## Heavy data processing

More and more organizations want to move lots of data around and put them to
good use use. Web frameworks alone simply aren't enough for this.

### Data Warehousing

Traditional data warehousing involves throwing all of a large company's
business data into a large relational database, and then expecting the database
to quickly field questions across about metrics like averages, correlations,
etc. For example, a grocery store chain might ask "how many apples were sold in
our northeast region in 2016?" The data warehouse would contain all the
transaction information across all the locations and crunch the numbers to spit
out the answer. On modern systems, this could reasonably take only a few
seconds. Cloud technologies such as [Amazon
Redshift](https://aws.amazon.com/redshift/) or on-premise technologies such as
[SAP Business
Warehouse](https://www.sap.com/product/data-mgmt/business-warehouse.html) are
engineered to quickly comb through large amounts of entries in a very efficient
way using specialized data storage methodologies such as [columnar
storage](https://en.wikipedia.org/wiki/Column-oriented_DBMS).  Web frameworks
do not have this ability. However, web frameworks can be used in tandem with
these systems in an integrated business intelligence solution to keep track of
queries, display dashboards, and provide user accounts.

### "Big Data Analytics" & "Machine Learning"

"Big Data" and "Machine Learning" may be among the most overloaded terms ever.
I will specify "Big Data" to mean operations on datasets that cannot fit on a
single computer, and "Machine Learning" to mean a combination of statistical
and probabilistic techniques that allow for predictive capabilities using less
human programming. These have significant overlap with data warehousing. For
example, a manufacturing company might have tens of thousands of equipment
installations that all need maintenance from a few centralized teams. A good
use of machine learning would be to first take measurements of many different
characteristics of these machines: total widgets produced since last
maintenance, temperature readings, historic failure rate, etc. These
characteristics can then be used to train a machine learning model to
accurately predict which machines need maintenance and when, saving the company
significant replacement costs through maintenance at the right time. As
mentioned above, web frameworks can be part of this solution, but the number
crunching and data processing are better left to other technologies meant for
this kind of use case such as [Apache Spark](https://spark.apache.org/),
[RStudio](https://www.rstudio.com/), and
[TensorFlow](https://www.tensorflow.org/).

## Situations with Expensive Failure Modes

The cost of mishandling a website request is pretty low. The user can just
refresh the page, and most of the time it's back. Maybe the application is down
for a few minutes--usually not a huge deal. But sometimes it is. You wouldn't
want an F-16 fighter plane to freeze up on its pilot in a dogfight, and you
wouldn't want a heart monitor to crash during a surgery. The stakes are far
higher in certain industries, and web software is incredibly complex and
error-prone compared to the standards that are necessary. These industries
include:

- Industrial Control Systems
- Military Applications
- Life Support Systems
- Automated Trading Systems

Companies in higher-stakes territories start to lean towards much smaller code
bases with far fewer dependencies than web software. This can be more easily
audited. They are also partial to
[statically-typed](https://en.wikipedia.org/wiki/Type_system#Static_typing)
programming languages that lead to far more predictable results than
dynamically-typed languages like Ruby and Python, and they have compilers that
perform series of checks to ensure correct behavior before producing a running
program. If you're trying to break into these industries, be prepared to shell
out a lot of money for very good programmers to write software that is correct
to a very certain degree, and expensive lawyers to pass regulations.

With that stern warning in mind, web frameworks can be and have been used to
great effect in healthcare systems, legal departments, and industrial control
analytic platforms.
