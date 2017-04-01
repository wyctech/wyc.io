<h2 id="intended-audience">Intended Audience</h2>
<p>You are a strapping entrepreneur who happens to be a little clueless when it comes to technology. But you have an idea involving software that can (1) change the world, (2) make you a lot of money, (3) be “pretty cool to just build”, or all of the above.</p>
<p><a name="features"></a></p>
<h2 id="need-any-of-these-for-your-product">Need any of these for your product? <a name="features" href="#features"><i class="linkify icon"></i></a></h2>
<p>While your idea may be unique, it’s very likely that the underlying components are actually pretty common. Do you need users to have accounts and profiles? Are you thinking about a rating system? Can people buy things on your website? These features are all assembled from similar building blocks. Below are some typical asks for new web products. Click each of them to expand, and read a summary and comparison between using a CMS and custom software via web framework:</p>
<div class="ui styled fluid accordion">
<div class="title">
<p><i class="dropdown icon"></i> Customizable User Accounts and Permissions</p>
</div>
<div class="content">
<div class="transition">
<p>
Allow users to log in with their Email/Google/Facebook/etc. accounts. Handle password resets and account details. Be able to specify finely-grained control over what they can see, edit, or share. Have user profiles that can be updated and displayed, easily linked to custom data models, and integrated into other software such as CRMs, support desks, other social networks, and more.
</p>
<h3>
What a CMS can do
</h3>
<p>
A CMS can render simple user profiles through additional plugins. Some great software packages such as <a href="https://buddypress.org/">BuddyPress</a> can bring much of this functionality in a very opinionated fashion–if you stray too far from the core objectives of these software packages, then you’ll run into more difficulties than if you just built a custom solution from the start.
</p>
<h3>
What Custom Software can do
</h3>
<p>
Custom software frameworks provide all of the described features out of the box, and there are countless integrations to many other services available through robust open-source libraries. For example, a seasoned Django developer would be able to easily integrate Salesforce with Eventbrite and sign up your website’s users for marketing campaigns and local event notifications depending on their demographics and interests.
</p>
</div>
</div>
<div class="title">
<p><i class="dropdown icon"></i> Interrelated Data Models</p>
</div>
<div class="content">
<div class="transition">
<p>
Cleanly represent your business data models and how they’re interconnected. For example, a pet store may have a data model representing a <code>Pet</code>, and another model called <code>MealPlan</code>. This feature could solve the problem of connecting each pet to its correct meal plan, allowing staff members to know when/what to feed each pet and then correctly log the the feeding activity.
</p>
<p>
In another example, a freight shipping company might need to quickly figure out what <code>Dock</code>s a <code>CargoVessel</code> will visit within the next 7 days. Interrelated data models are used to link these models together and provide a very straightforward way to ask about things like delivery times, total value of deliveries per vessel, volume over time, and more.
</p>
<p>
<p>More examples include:</p>
<ul>
<li>A star rating system with reviews and additional functionality per review</li>
<li>A list of friends connected through different relationships</li>
<li>Multiple interacting profiles such as rental listings, renters, and landlords</li>
<li>A delivery service where users can see a driver’s location in real time</li>
</ul>
</p>
<h3>
What a CMS can do
</h3>
<p>
A CMS is first and foremost meant to publish written content. The name <i>Content Management System</i> does not suggest otherwise. Therefore, most CMS packages are not great at representing anything other than posts, comments, and perhaps events. For example, WordPress has plugins that attempt to contrive core models such as Post into a desired new data model, but these tend to be unreliable. Other CMS packages such as Drupal have more sophisticated support for what’s known as <i>custom content types</i>, but these add-ons are hardly covered by the core contributing members, and may bring frustrating functionality, performance, and security issues.
</p>
<p>
There exist some add-on packages that can transform CMSes for very specific use cases. There are some for turning WordPress into an eCommerce store, course registration platform, or landing page. However, the data models that these specialized add-ons rely on have a very established way of functioning, and you should only consider using them if they fit your use case nearly perfectly.
</p>
<h3>
What Custom Software can do
</h3>
<p>
This is frankly the bread and butter of many custom software frameworks. The core components in most web frameworks involve providing the developer easy ways to model and interconnect real business processes. For example, Django’s <a href="https://docs.djangoproject.com/en/1.10/topics/db/"><code>db</code> module</a>, Rail’s <a href="https://github.com/rails/rails/tree/master/activerecord">ActiveRecord</a>, and Laravel’s <a href="https://laravel.com/docs/5.4/eloquent">Eqloquent</a>. This is easily a central competency for custom software development, and if you have a lot of interrelated data models, then you should consider skipping CMSes and jumping right into custom development.
</p>
</div>
</div>
<div class="title">
<p><i class="dropdown icon"></i> Background Processing</p>
</div>
<div class="content">
<div class="transition">
<p>
<p>Your business may need to perform tasks that involve background processing. Such tasks do not involve a user clicking on a link, and should happen automatically in the background. These are timed or recurring events, or tasks that would take too long to do in a page load for the user: services such as automatically syncing to Dropbox, queuing emails to send later, crawling web pages, and running analytics.</p>
<p>Some real life examples:</p>
<ul>
<li>Compute a summary of the daily sales at 12AM every night, and send them to Google Sheets along with an email notification.</li>
<li>Check a list of websites for new events every 20 minutes, and if there is a new event anywhere, then add it to our collection of aggregated events.</li>
<li>Remind a user that their document is awaiting their signature, and then notify customer service if they’re unresponsive for two more business days after that.</li>
</ul>
</p>
<h3>
What a CMS can do
</h3>
<p>
CMS software is not geared for background processing through the user interface. This involves custom CMS programming and/or use of plugins with very narrow functionality.
</p>
<h3>
What Custom Software can do
</h3>
<p>
Web frameworks such as Django or Rails have very robust background job systems that even include reporting and retrying jobs that have failed. Django has <a href="https://github.com/celery/celery">Celery project</a>, and Rails has <a href="https://github.com/resque/resque">Resque</a>. These are very mature systems that have handled traffic at Internet scale and are routinely used by top technology companies.
</p>
</div>
</div>
<div class="title">
<p><i class="dropdown icon"></i> Flexible Service Integrations</p>
</div>
<div class="content">
<div class="transition">
<p>
<p>It’s likely that your business will need to talk to other services. Perhaps it needs to place a new order of inventory to fulfill a customer’s order, or maybe it should keep track of a user’s activity in a CRM such as Salesforce or Hubspot. What about integrating with an existing login system? These are all situations where your application is expected to have a deep service integration to perform its essential business process.</p>
</p>
<h3>
What a CMS can do
</h3>
<p>
<p>CMS packages will fall short here unless you need a very popular integration that has explicit support from the community. One such example is a plugin for WordPress or Drupal that can funnel contact information from a landing page into Salesforce. However, in this example, using Hubspot instead of Salesforce would likely require a different plugin due to the rigidity of this integration.</p>
Some intermediate business automation solutions such as <a href="https://zapier.com/">Zapier</a> and <a href="https://ifttt.com/">IFTTT</a> do exist to gap the bridge of very specific plugins and custom programming, and tend to be wonderful solutions if the integrations are present on their platforms. However, these tools usually are limited by what the CMS decides to expose for integration.
</p>
<h3>
What Custom Software can do
</h3>
<p>
Custom software can integrate with anything that can be communicated with. Not only can custom software leverage web frameworks, but it can also use any other developments within the entire ecosystem of the its programming language. These ecosystems are massive, and popular web programming langauges such as Python, Ruby, and PHP tend to have official packages available and supported from many vendors, which will reduce the amount of time and effort required from a developer.
</p>
</div>
</div>
<div class="title">
<p><i class="dropdown icon"></i> Mobile App or Single Page App Backend</p>
</div>
<div class="content">
<div class="transition">
<p>
<p>Your business might be served best by a highly-responsive mobile web page or a native mobile application for iOS or Android available on the Play or App stores. Do you have to build the whole thing from scratch again to support these types of pages?</p>
</p>
<h3>
What a CMS can do
</h3>
<p>
With a CMS, you will likely not be able to reuse any of the work done so far, and you must then begin the pursuit of a completely new solution to support your single page or mobile apps. Support for native mobile apps using the CMS as a backend can be done, but the road is long and treacherous. Many developers will likely charge even more for this option as it’s very much like trying to get a stubborn mule to do ballet.
</p>
<p>
The problem is that CMSes aren’t designed to satisfy the needs of interacting with a mobile application. In the overwhelming majority of cases, it’s better to just start custom software development. If mobile apps are your main product, then a service such as <a href="https://firebase.google.com/">Firebase</a> by Google can usually save a lot of developer time.
</p>
<h3>
What Custom Software can do
</h3>
<p>
Custom web software happily serves as a reliable and scalable backend for a mobile application. For example, LinkedIn built their mobile app backend using Ruby on Rails, and it supports millions of concurrently active users. One great benefit of developing custom web software is that the foundation will have been set for the development of mobile applications.
</p>
<p>
If you must support both web and mobile, then this is the clear choice.
</p>
</div>
</div>
</div>
<p><br/> Maybe you could find uses for all of these features, as most companies chasing custom software do.</p>
<h2 id="no-i-dont-even-need-most-of-those">No, I don’t even need most of those!</h2>
<p>If you’re still scratching your head over how some or even most of these features could possibly help to your business, then maybe you don’t need custom software after all and could be better served with a content management system such as WordPress or Drupal. These solutions require no coding to get started and are also free and open-source, and with enough effort, can approach feature-parity to web frameworks such as Django or Rails. However, this would be like trying to make new furniture by modifying existing IKEA furniture. It can be done, and sometimes it actually works quite well, but often it ends up becoming Frankenstein’s monster.</p>
<p>To be clear, there’s nothing wrong with a WordPress or Drupal site that has grown to support your business. There are many companies that have started with CMS software and, through their success, funded a series of modifications to better serve their new and existing needs. However, CMS software was designed to achieve the specific goal of content publishing, and if you stray too far from this core pattern, then often you end up maintaining software far more complicated than the same thing built with a web framework such as Django or Rails. Riding a CMS makes functionality changes and new features a lot more expensive, and at this point many companies opt to rebuild their applications entirely. If you see now that your business will definitely need the features above, then you could save a lot of pain now by getting it built to your needs via a web framework. Otherwise, a CMS may serve your business well enough to the point of being able to afford a web framework-based solution, or perhaps it’s such a great fit that you’ll never need to switch.</p>
<h2 id="yes-i-need-most-of-those-to-execute-my-core-business-model.">Yes, I need most of those to execute my core business model.</h2>
<p>If all or most of these could help your business, then you’re in luck; the dominant web frameworks and their accompanying ecosystems of add-on packages bring those to the table for free!</p>
<p>Is it possible to instead build everything from scratch so that every single line of code perfectly suits your needs? Probably, but your rendition won’t be crafted by the same world-class web framework developers or battle-tested for over a decade. The benefits of building everything yourself tend to be marginal if present at all. It also won’t be free. You’ll end up paying a lot of money for what amounts to a bad version of already existing web frameworks.</p>
<p>Moreover, teams developing new products must spend their time actually talking to customers and figuring out where the real problems are in their industries. If something’s not part of the core value that you’re providing, then it’s usually better to buy or borrow than to build. Everything that you’re not competing or differentiating on should be made standard. The costs of standardized components race towards zero.</p>
<p>Thanks to the wonderful open-source movement and community, these projects come at no cost to you. If you’re making healthy profits off of these projects, you could consider giving back:</p>
<ul>
<li><a href="https://www.djangoproject.com/fundraising/" class="uri">https://www.djangoproject.com/fundraising/</a></li>
<li><a href="https://github.com/rails/rails#contributing" class="uri">https://github.com/rails/rails#contributing</a></li>
</ul>
<h2 id="exceptions">Exceptions</h2>
<p>All rules have exceptions. There are definitely situations where Rails and Django may not be the first choices for use in custom software development.</p>
<p>See the blog post titled <a href="/posts/when-not-to-use-a-web-framework/">When Not to Use a Web Framework</a>.</p>