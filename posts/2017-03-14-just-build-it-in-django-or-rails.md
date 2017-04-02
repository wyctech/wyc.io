---
title: "Just Build it in Django or Rails"
postUrl: /posts/just-build-it-in-django-or-rails/
imageUrl: "/images/django_rails.jpg"
description: "
<p>Many companies fret about what technologies they should use to build their
custom web and mobile backend software. It's easy to see why, given the
dizzying array of choices that all claim they're the best; and the claims may
all be true for certain specific use cases. To the uninitiated, it's a keyword
soup of terms like JavaScript, Python, Ruby, ReactJS, Node.js, AngularJS, and
Elixir. However, it doesn't have to be complicated. <b>If you care most about
spending your time actually talking to customers, then just build it in Django
or Rails.</b></p>
"
featuredOrder: 9999
relatedPosts:
    - "2017-01-20-when-not-to-use-a-web-framework.md"
    - "2017-02-21-wyc-helps-ibm-patch-security-vulnerability.md"
---

So you decided that you need custom software and not [something like a
CMS](/posts/cms-or-custom-software/). What web framework do you use to speed up
development? If you don't use one at all, then you'll most likely be sinking
unecessary time and money into a poor version of one. To quote a [previous
post](/posts/cms-or-custom-software/):

<blockquote>
&ldquo;Is it possible to instead build everything from scratch so that every
single line of code perfectly suits your needs? Probably, but your rendition
won't be crafted by the same world-class web framework developers or
battle-tested for over a decade.  The benefits of building everything yourself
tend to be marginal if present at all. It also won't be free. You'll end up
paying a lot of money for what amounts to a bad version of already existing web
frameworks.&rdquo;
</blockquote>

If you're not opinionated about this decision, then as the title suggests, in
2017 you just can't go wrong with choosing either [the Django
project](https://www.djangoproject.com/) or [Ruby on
Rails](http://rubyonrails.org/) as the primary web framework for building your
applications.

Of course, every company has different needs, but these frameworks are crafted
in a modular form so you can easily pull in or cut out components, leaving only
the perfect solution for your business needs. Even the most customized race car
is a variation on the standard model of a car; it has wheels, a chassis, a
transmission, and an engine. Web applications are the same way, and good
frameworks such as Django or Rails bring all the parts you could want and a
basic functioning skeleton to modify and build on.

## Versus Other Web Frameworks

What makes Django and Rails so special? It's a combination of feature sets,
community, and track record over time.

### The Right Features Made to Work Together

Some web frameworks or toolkits are Bring Your Own X, where X is anything from
data modeling layers, to HTML rendering engines, to authentication systems, to
database migration managers. These kinds of frameworks typically boast
customizability and simplicity, and rightfully so. However, with extensive
customizability also comes extensive effort to get your project to do what you
want, meaning additional development hours and project management.

Examples include:

- [Flask](http://flask.pocoo.org/)
- [Sinatra](http://www.sinatrarb.com/)
- [Gorilla](http://www.gorillatoolkit.org/)

These are great if you have some very custom or slim application in mind,
perhaps one of the use cases described in the post [When Not to Use a Web
Framework](/posts/when-not-to-use-a-web-framework). However, if you're starting
a business or implementing a new web product for your existing business, then a
more comprehensive web framework will probably better serve you. To find out if
this is indeed the case, you can visit the aforementioned link to read the
guide.

In particular, Rails and Django both have all the following components built-in
and playing nicely together:

- **User Accounts and Permissions** to let your users log in and have profiles.
  These frameworks have solid extensions that have shown to be reasonably
  secure building blocks, which is about as secure as you could hope for.
- **Mappings From Business to Software (Models)** to represent the actors at
  play in your business. Most web applications end up cobbling together
  something similar from scratch, but not as robust. Take a look at [how
  clear it is to express your data models](https://docs.djangoproject.com/en/1.10/topics/db/models/#quick-example). You don't even need to know how to code to mostly
  know what's there.
- **Management of Database Schemas** to modify business data models and the
  relationships between them. If you decide that a user should have many user
  profiles instead of just one, it's not a problem to change.
- **Integrated HTML Rendering (Views)** to customize the look and feel of your
  application without compromising on advanced functionality just to make
  things look pretty.
- **Administrator Interfaces** for editing the data in your application as an
  administrator without building out a separate website for administrators and
  company employees.
- **"Realtime" Events** served via [Django
  Channels](https://channels.readthedocs.io/en/stable/) or [Rails
  ActionCable](https://github.com/rails/rails/tree/master/actioncable). Send
  notifications to your users when they receive new messages, and receive
  information without having them refresh the page.

### Community and Ecosystem

The community and ecosystem are together an important (if not the most
important) aspect of choosing your technology stack. They will dictate
how easy it is to hire competent technologists, how easy it is for your
technologists to solve their problems, and how your organization is enabled
to grow and evolve moving forward. Django and Rails both have phenomenal
communities and ecosystems.

#### Finding Developers

There are reasons why developer bootcamps will teach their students one of
these two frameworks. As described above, these frameworks are well-crafted and
in wide use. Employers want to use it, and employees want to have jobs. The
flywheel is already spinning very quickly. A quick Google search will yield
results for job boards specifically for Django and Rails opportunities--this is
how prevalent these technologies are. This means that there's a market, and
you'll get a fair rate for a competent developer.

The more popular the technology, the easier it is to hire and find help for.
This is why Java is still very popular; it's even making a comeback on web
(more on this in a future post). Good luck finding a business-savvy Haskell
developer in 2017 willing to contract work for a rate that you can afford.

#### Libraries

If you need to integrate into Salesforce, Mailchimp, or anything else, it's
likely that someone has already solved that problem specifically for one of
these two frameworks. Furthermore, it's likely that they've made their solution
freely available and ready for you to use in the form of a software library.

Because of their massive usefulness, software libraries are an important metric
in measuring how stable and easy to use a technology would be. There are
literally thousands of free add-ons and packages per Django or Rails.  At the
time of writing, [Django Packages](https://djangopackages.org/) counts over
3,000 for Django, and there are probably a similar number for Rails at [Ruby
Toolbox](https://www.ruby-toolbox.com/). These are only the framework-specific
libraries, too.

Any code written for these frameworks is able to leverage all the libraries
written for its respective programming language, and the amount of freely
available quality Python/Ruby libraries is staggering.

### Technology Maturity

These frameworks have been battle-tested in trials ranging from highly-skilled
hackers trying to break them, to the trifles of Internet scale in serving
millions of users at once, to handling complex business models, to deployments
in a unbelievable number of crazy different environments.

This means that the path is very well-beaten, and a developer is unlikely to
encounter many thorns aside from their own ignorance. Most of the bugs have
been squashed (and you can never really squash all of them).

Django started in 2005, 11 years ago as of the time of this writing. It has
since had over a thousand people make changes to its code base and even more
scrutinizing it for imperfections. Rails started in 2004, 12 years ago, and has
had over three thousand contributers, also scrutinizing and making changes.
Both are solid pieces of technology to be reckoned with.


## Examples in the Wild

To give a feel of how rock-solid Django and Rails are, below is a compiled list
of companies built on it for 2017. If these frameworks are good enough for
these world-class traffic-heavy and feature-rich products, then it's likely
good enough for yours.

## Companies Based on Django

### [Instagram](https://www.instagram.com/)

Instagram is a wildly-popular photo sharing service with more than 600 million
users. There are more than 80 million photos uploaded and 3.5 billion likes per
day. It still runs on Django, which proved stable and modular enough to allow
the engineering team to swap out the database to a more customized solution
when the company was experiencing growing pains. A benefit of a well-engineered
and modular piece of software such as Django is that it can be customized in a
straight-forward manner to grow with your organization.

[<i class="linkify icon"></i>Mike Krieger on Scaling Instagram](http://files.cnblogs.com/files/bitwolaiye/Mike-Krieger-Instagram-at-the-Airbnb-tech-talk-on-Scaling-Instagram.pdf)

[<i class="linkify icon"></i>Stackshare - Instagram](https://stackshare.io/instagram/instagram)

### [Pinterest](https://www.pinterest.com/)

Pinterest is a media-rich sharing platform where users can share and save
recipes, home decor ideas, and more. It averages 5 million posts per day to its
platform, and many more comments and saves. Pinterest started on Django, and
through its growth, has also made many heavy customizations to the framework
including a sharded database solution and custom search indexes. This is
another example of the framework providing value to the business from day one,
and then being able to rapidly add supporting capabilities as needed over the
span of years.

[<i class="linkify icon"></i>High Scalability - Scaling Pinterest](http://highscalability.com/blog/2013/4/15/scaling-pinterest-from-0-to-10s-of-billions-of-page-views-a.html)

[<i class="linkify icon"></i>Stackshare - Pinterest](https://stackshare.io/pinterest/pinterest)

### [Disqus](https://disqus.com/)

Disqus is a plug-and-play discussion and engagement tool that processes more
than 165,000 requests per second as of 2014. A pure-Django solution got the
company to 45,000 requests per second, and then they switched out a tiny
component into the Go programming language to handle real-time message
processing (see the exceptions section above).  However, the rest of the
architecture was still written in Django, as of 2014. These web frameworks work
well as part of a wholesome solution, and they provide a very nice way to
specify business data models even well into a company's maturity.

[<i class="linkify icon"></i>Scaling Django to 8 Billion Page Views](https://blog.disqus.com/scaling-django-to-8-billion-page-views)

[<i class="linkify icon"></i>High Scalability - Disqus](http://highscalability.com/blog/2014/4/28/how-disqus-went-realtime-with-165k-messages-per-second-and-l.html)

[<i class="linkify icon"></i>Stackshare - Disqus](https://stackshare.io/disqus/disqus)

### [Bitbucket](https://bitbucket.org)

Bitbucket is Atlassian's competitor to GitHub. It is also an Internet hosting
service that serves open-source software that is smaller in usage, but still
serves projects in the millions. It has been tightly integrated into
Atlassian's other service offerings including the JIRA issue tracker and the
Confluence project management tool. This is an example of how nicely Django can
play well with other systems and services at a large scale.

[<i class="linkify icon"></i>Bitbucket - Wikipedia](https://en.wikipedia.org/wiki/Bitbucket)

[<i class="linkify icon"></i>Atlassian](https://www.atlassian.com/)


## Companies Based on Ruby on Rails

### Twitter

Twitter started in 2006, and used Rails successfully for five years before
making the switch to a different JVM-based platform in 2011. They identified a
massive architectural benefit to their business by switching to a
statically-typed language (Scala) and framework (Play) that was in many ways a
better natural fit for their messaging platform.  At this point, they already
had many exceptional engineering hires and huge amounts of traffic. Their
business strategy and core engineering problems had been identified, and Rails
got the job done until that point. Reports suggest that Twitter still uses both
Django and Rails on new projects.

[<i class="linkify icon"></i>Twitter Shifting More Code to JVM, Citing Performance and Encapsulation As Primary Drivers](https://www.infoq.com/articles/twitter-java-use)

[<i class="linkify icon"></i>Twitter Search is Now 3x Faster](https://blog.twitter.com/2011/twitter-search-is-now-3x-faster)

[<i class="linkify icon"></i>Stackshare - Twitter](https://stackshare.io/twitter/twitter)


### Shopify

Shopify is an ecommerce platform. In 2016, it processed $15.4 billion in Gross
Merchandise Volume with annual revenues of near $400 million. It's still
running on Rails, and has managed to scale Rails to support over 375,000
merchants. Rails has been proven to be robust and performant enough to create a
platform that supports hundreds of thousands of ecommerce businesses, which can
afford little to no downtime.

[<i class="linkify icon"></i>How Shopify Scales Rails](https://www.slideshare.net/jduff/how-shopify-scales-rails-20443485)

[<i class="linkify icon"></i>Shopify Announces Fourth-Quarter and Full Year 2016 Financial Results](https://www.shopify.com/press/releases/shopify-announces-fourth-quarter-and-full-year-2016-financial-results)

[<i class="linkify icon"></i>Stackshare - Shopify](https://stackshare.io/shopify/shopify)


### Airbnb

Airbnb is an online marketplace for hospitality. People stay at privately-owned
apartments, homes, etc. for reasonable fees. Estimates put its revenues at far
over $1 billion for 2016, and in 2015, it had over 80 million bookings. The
number of bookings is likely to be far higher today, and they still rely on
Ruby on Rails. Due to its excellent engineering team, Airbnb has managed to
scale Ruby on Rails to reliably and securely process all the accompanying
payments for those bookings.  Not only is Rails great for prototyping, but it
can be made even more robust in the necessary ways when it's needed.

[<i class="linkify icon"></i>Large Scale Payments Systems and Ruby on Rails](http://nerds.airbnb.com/large-scale-payments-systems-ruby-rails/)

[<i class="linkify icon"></i>Airbnb's revenue soars 89 percent](http://www.bizjournals.com/sanfrancisco/news/2016/09/01/airbnbs-revenue-soars-compared-to-hotels.html)

[<i class="linkify icon"></i>Stackshare - Airbnb](https://stackshare.io/airbnb/airbnb)


### GitHub

GitHub is an Internet hosting service that serves much of the world's
open-source software. Interestingly, Rails and Django source code repositories
are both hosted on GitHub. There are over 50 million other projects hosted on
GitHub today, and it is still being served by a scaled up Rails installation.
Large and small software companies rely completely on GitHub every day to
manage their code repositories. This speaks to how rock-solid a Rails
application can be made.

[<i class="linkify icon"></i>GitHub: Scaling on Ruby, with a nomadic tech team](https://medium.com/s-c-a-l-e/github-scaling-on-ruby-with-a-nomadic-tech-team-4db562b96dcd#.tl56tya20)

[<i class="linkify icon"></i>Stackshare - GitHub](https://stackshare.io/github/github)

