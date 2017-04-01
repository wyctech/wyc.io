<p>So you decided that you need custom software and not <a href="/posts/cms-or-custom-software/">something like a CMS</a>. What web framework do you use to speed up development? If you don’t use one at all, then you’ll most likely be sinking unecessary time and money into a poor version of one. To quote a <a href="/posts/cms-or-custom-software/">previous post</a>:</p>
<blockquote>
“Is it possible to instead build everything from scratch so that every single line of code perfectly suits your needs? Probably, but your rendition won’t be crafted by the same world-class web framework developers or battle-tested for over a decade. The benefits of building everything yourself tend to be marginal if present at all. It also won’t be free. You’ll end up paying a lot of money for what amounts to a bad version of already existing web frameworks.”
</blockquote>
<p>If you’re not opinionated about this decision, then as the title suggests, in 2017 you just can’t go wrong with choosing either <a href="https://www.djangoproject.com/">the Django project</a> or <a href="http://rubyonrails.org/">Ruby on Rails</a> as the primary web framework for building your applications.</p>
<p>Of course, every company has different needs, but these frameworks are crafted in a modular form so you can easily pull in or cut out components, leaving only the perfect solution for your business needs. Even the most customized race car is a variation on the standard model of a car; it has wheels, a chassis, a transmission, and an engine. Web applications are the same way, and good frameworks such as Django or Rails bring all the parts you could want and a basic functioning skeleton to modify and build on.</p>
<h2 id="versus-other-web-frameworks">Versus Other Web Frameworks</h2>
<p>What makes Django and Rails so special? It’s a combination of feature sets, community, and track record over time.</p>
<h3 id="the-right-features-made-to-work-together">The Right Features Made to Work Together</h3>
<p>Some web frameworks or toolkits are Bring Your Own X, where X is anything from data modeling layers, to HTML rendering engines, to authentication systems, to database migration managers. These kinds of frameworks typically boast customizability and simplicity, and rightfully so. However, with extensive customizability also comes extensive effort to get your project to do what you want, meaning additional development hours and project management.</p>
<p>Examples include:</p>
<ul>
<li><a href="http://flask.pocoo.org/">Flask</a></li>
<li><a href="http://www.sinatrarb.com/">Sinatra</a></li>
<li><a href="http://www.gorillatoolkit.org/">Gorilla</a></li>
</ul>
<p>These are great if you have some very custom or slim application in mind, perhaps one of the use cases described in the post <a href="/posts/when-not-to-use-a-web-framework">When Not to Use a Web Framework</a>. However, if you’re starting a business or implementing a new web product for your existing business, then a more comprehensive web framework will probably better serve you. To find out if this is indeed the case, you can visit the aforementioned link to read the guide.</p>
<p>In particular, Rails and Django both have all the following components built-in and playing nicely together:</p>
<ul>
<li><strong>User Accounts and Permissions</strong> to let your users log in and have profiles. These frameworks have solid extensions that have shown to be reasonably secure building blocks, which is about as secure as you could hope for.</li>
<li><strong>Mappings From Business to Software (Models)</strong> to represent the actors at play in your business. Most web applications end up cobbling together something similar from scratch, but not as robust. Take a look at <a href="https://docs.djangoproject.com/en/1.10/topics/db/models/#quick-example">how clear it is to express your data models</a>. You don’t even need to know how to code to sort of know what’s there.</li>
<li><strong>Management of Database Schemas</strong> to modify business data models and the relationships between them. If you decide that a user should have many user profiles instead of just one, it’s not a problem to change.</li>
<li><strong>Integrated HTML Rendering (Views)</strong> to customize the look and feel of your application without compromising on advanced functionality just to make things look pretty.</li>
<li><strong>Administrator Interfaces</strong> for editing the data in your application as an administrator without building out a separate website for administrators and company employees.</li>
<li><strong>“Realtime” Events</strong> served via <a href="https://channels.readthedocs.io/en/stable/">Django Channels</a> or <a href="https://github.com/rails/rails/tree/master/actioncable">Rails ActionCable</a>. Send notifications to your users when they receive new messages, and receive information without having them refresh the page.</li>
</ul>
<h3 id="community-and-ecosystem">Community and Ecosystem</h3>
<h3 id="technology-maturity">Technology Maturity</h3>
<h2 id="examples-in-the-wild">Examples in the Wild</h2>
<p>To give a feel of how rock-solid Django and Rails are, below is a compiled list of companies built on it for 2017. If these frameworks are good enough for these world-class traffic-heavy and feature-rich products, then it’s likely good enough for yours.</p>
<h2 id="companies-based-on-django">Companies Based on Django</h2>
<h3 id="instagram"><a href="https://www.instagram.com/">Instagram</a></h3>
<p>Instagram is a wildly-popular photo sharing service with more than 600 million users. There are more than 80 million photos uploaded and 3.5 billion likes per day. It still runs on Django, which proved stable and modular enough to allow the engineering team to swap out the database to a more customized solution when the company was experiencing growing pains. A benefit of a well-engineered and modular piece of software such as Django is that it can be customized in a straight-forward manner to grow with your organization.</p>
<p><a href="http://files.cnblogs.com/files/bitwolaiye/Mike-Krieger-Instagram-at-the-Airbnb-tech-talk-on-Scaling-Instagram.pdf"><i class="linkify icon"></i>Mike Krieger on Scaling Instagram</a></p>
<p><a href="https://stackshare.io/instagram/instagram"><i class="linkify icon"></i>Stackshare - Instagram</a></p>
<h3 id="pinterest"><a href="https://www.pinterest.com/">Pinterest</a></h3>
<p>Pinterest is a media-rich sharing platform where users can share and save recipes, home decor ideas, and more. It averages 5 million posts per day to its platform, and many more comments and saves. Pinterest started on Django, and through its growth, has also made many heavy customizations to the framework including a sharded database solution and custom search indexes. This is another example of the framework providing value to the business from day one, and then being able to rapidly add supporting capabilities as needed over the span of years.</p>
<p><a href="http://highscalability.com/blog/2013/4/15/scaling-pinterest-from-0-to-10s-of-billions-of-page-views-a.html"><i class="linkify icon"></i>High Scalability - Scaling Pinterest</a></p>
<p><a href="https://stackshare.io/pinterest/pinterest"><i class="linkify icon"></i>Stackshare - Pinterest</a></p>
<h3 id="disqus"><a href="https://disqus.com/">Disqus</a></h3>
<p>Disqus is a plug-and-play discussion and engagement tool that processes more than 165,000 requests per second as of 2014. A pure-Django solution got the company to 45,000 requests per second, and then they switched out a tiny component into the Go programming language to handle real-time message processing (see the exceptions section above). However, the rest of the architecture was still written in Django, as of 2014. These web frameworks work well as part of a wholesome solution, and they provide a very nice way to specify business data models even well into a company’s maturity.</p>
<p><a href="https://blog.disqus.com/scaling-django-to-8-billion-page-views"><i class="linkify icon"></i>Scaling Django to 8 Billion Page Views</a></p>
<p><a href="http://highscalability.com/blog/2014/4/28/how-disqus-went-realtime-with-165k-messages-per-second-and-l.html"><i class="linkify icon"></i>High Scalability - Disqus</a></p>
<p><a href="https://stackshare.io/disqus/disqus"><i class="linkify icon"></i>Stackshare - Disqus</a></p>
<h3 id="bitbucket"><a href="https://bitbucket.org">Bitbucket</a></h3>
<p>Bitbucket is Atlassian’s competitor to GitHub. It is also an Internet hosting service that serves open-source software that is smaller in usage, but still serves projects in the millions. It has been tightly integrated into Atlassian’s other service offerings including the JIRA issue tracker and the Confluence project management tool. This is an example of how nicely Django can play well with other systems and services at a large scale.</p>
<p><a href="https://en.wikipedia.org/wiki/Bitbucket"><i class="linkify icon"></i>Bitbucket - Wikipedia</a></p>
<p><a href="https://www.atlassian.com/"><i class="linkify icon"></i>Atlassian</a></p>
<h2 id="companies-based-on-ruby-on-rails">Companies Based on Ruby on Rails</h2>
<h3 id="twitter">Twitter</h3>
<p>Twitter started in 2006, and used Rails successfully for five years before making the switch to a different JVM-based platform in 2011. They identified a massive architectural benefit to their business by switching to a statically-typed language (Scala) and framework (Play) that was in many ways a better natural fit for their messaging platform. At this point, they already had many exceptional engineering hires and huge amounts of traffic. Their business strategy and core engineering problems had been identified, and Rails got the job done until that point. Reports suggest that Twitter still uses both Django and Rails on new projects.</p>
<p><a href="https://www.infoq.com/articles/twitter-java-use"><i class="linkify icon"></i>Twitter Shifting More Code to JVM, Citing Performance and Encapsulation As Primary Drivers</a></p>
<p><a href="https://blog.twitter.com/2011/twitter-search-is-now-3x-faster"><i class="linkify icon"></i>Twitter Search is Now 3x Faster</a></p>
<p><a href="https://stackshare.io/twitter/twitter"><i class="linkify icon"></i>Stackshare - Twitter</a></p>
<h3 id="shopify">Shopify</h3>
<p>Shopify is an ecommerce platform. In 2016, it processed $15.4 billion in Gross Merchandise Volume with annual revenues of near $400 million. It’s still running on Rails, and has managed to scale Rails to support over 375,000 merchants. Rails has been proven to be robust and performant enough to create a platform that supports hundreds of thousands of ecommerce businesses, which can afford little to no downtime.</p>
<p><a href="https://www.slideshare.net/jduff/how-shopify-scales-rails-20443485"><i class="linkify icon"></i>How Shopify Scales Rails</a></p>
<p><a href="https://www.shopify.com/press/releases/shopify-announces-fourth-quarter-and-full-year-2016-financial-results"><i class="linkify icon"></i>Shopify Announces Fourth-Quarter and Full Year 2016 Financial Results</a></p>
<p><a href="https://stackshare.io/shopify/shopify"><i class="linkify icon"></i>Stackshare - Shopify</a></p>
<h3 id="airbnb">Airbnb</h3>
<p>Airbnb is an online marketplace for hospitality. People stay at privately-owned apartments, homes, etc. for reasonable fees. Estimates put its revenues at far over $1 billion for 2016, and in 2015, it had over 80 million bookings. The number of bookings is likely to be far higher today, and they still rely on Ruby on Rails. Due to its excellent engineering team, Airbnb has managed to scale Ruby on Rails to reliably and securely process all the accompanying payments for those bookings. Not only is Rails great for prototyping, but it can be made even more robust in the necessary ways when it’s needed.</p>
<p><a href="http://nerds.airbnb.com/large-scale-payments-systems-ruby-rails/"><i class="linkify icon"></i>Large Scale Payments Systems and Ruby on Rails</a></p>
<p><a href="http://www.bizjournals.com/sanfrancisco/news/2016/09/01/airbnbs-revenue-soars-compared-to-hotels.html"><i class="linkify icon"></i>Airbnb’s revenue soars 89 percent</a></p>
<p><a href="https://stackshare.io/airbnb/airbnb"><i class="linkify icon"></i>Stackshare - Airbnb</a></p>
<h3 id="github">GitHub</h3>
<p>GitHub is an Internet hosting service that serves much of the world’s open-source software. Interestingly, Rails and Django source code repositories are both hosted on GitHub. There are over 50 million other projects hosted on GitHub today, and it is still being served by a scaled up Rails installation. Large and small software companies rely completely on GitHub every day to manage their code repositories. This speaks to how rock-solid a Rails application can be made.</p>
<p><a href="https://medium.com/s-c-a-l-e/github-scaling-on-ruby-with-a-nomadic-tech-team-4db562b96dcd#.tl56tya20"><i class="linkify icon"></i>GitHub: Scaling on Ruby, with a nomadic tech team</a></p>
<p><a href="https://stackshare.io/github/github"><i class="linkify icon"></i>Stackshare - GitHub</a></p>