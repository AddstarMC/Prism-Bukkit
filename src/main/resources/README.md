### Contributing

Prism is open source and we're accepting pull requests via GitHub. If you'd like to contribute, please read the following:

- We accept work via GitHub pull requests. 
- We'll try to review your PR as soon as possible.
- Please try to keep your code clean, and try to follow the formatting Prism uses already.
- Every pull request **must** refer to a ticket in our bug tracker. Snowy supports bug mentions in commit messages like "bug 25" or "bugs 25, 26" and a wide variety of other formats.
- If your PR would fully resolve a ticket, please use "bug 25:resolved" (with the proper bug number of course).
- We strongly recommend you hang out with us in IRC (irc.esper.net - #prism). Viveleroi and nasonfish are in there quite often, as well as other dev build testers from DHMC.


### Our Branch Management Process

This may change if our needs to, but currently:

- All non-urgent development towards the next release is done on `master`
- We tag the repo for each release type.
- Urgent bug fixes are applied to branches made from a release's tag. For example, 1.1 was tagged and released. Urgent bug fixes for 1.1.1 we're done on the branch `1.1`.
- Work planned for each release are tracked in Snowy by milestones matching the release numbers.
- A very basic release script allows me to throw dev builds when needed up to S3, with a list running on our website: http://discover-prism.com/dev/builds. I'm not a huge fan of Jenkins but depending on how development proceeds we'll make decisions for build versions later.


### Just a Note

Having a run a busy public server for eighteen months, I had a long wanted to write a rollback plugin that truly solved our needs. We've been through multiple rollback plugins but were never satisfied, and unfortunately fairly displeased with some. 

A day before New Years I began an experiment for such a plugin - testing the waters. Initial work went very well and I saw a clear path ahead. Over the next several weeks work progressed at a rapid pace, and soon several staff members from our servers were jumping at the chance to test and see its progression.

Our road to 1.0 was extremely successful and by 1.1 we're already running on 300+ servers a day. I have yet to hear anyone who's tried it do anything other than praise it or say it won't work for their needs. 

Prism shows so much potential, it makes sense to open source it. Despite releasing a lot of work as open source in the past, I've been hesitant to open Prism for a number of likely warrant-less personal concerns. However, I recognize that Prism needs the benefit of contributions from our rapidly growing community.

If you're interested in contributing to Prism, please read the following. THANK YOU!
