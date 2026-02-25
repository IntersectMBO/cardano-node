# Security Vulnerability Disclosure Policy

## Introduction

The Cardano open source project (xxx) is committed to ensuring the security of
its software and the privacy of its users. We value the contributions
of the security community in helping us identify and address
vulnerabilities in our code. This Security Vulnerability Disclosure
Policy outlines how security vulnerabilities should be reported and
how we will respond to and remediate such reports.

## Security Vulnerability Handling Process

### Reporting a Vulnerability

If you discover a security vulnerability in xxxx, we encourage you to
responsibly disclose it to us. To report a vulnerability, please use
the [private reporting form on
GitHub](https://github.com/input-output-hk/mithril/security/advisories/new)
to draft a new _Security advisory_.

Please include as much details as needed to clearly qualify the issue:

- A description of the vulnerability and its potential impact.
- Steps to reproduce the vulnerability.
- The version of `xxxx` package where the vulnerability exists.
- Any relevant proof-of-concept or exploit code (if applicable).

### Processing Vulnerability

1. **Acknowledgment**: The team acknowledges the receipt of your report
   within 3 business days by commenting on the issue reporting it or replying to email.

2. **Validation**: The team investigates the issue and either _reject_ or _validate_ the
   reported vulnerability.

   a. **Rejection**: If the team rejects the report, detailed explanations will be provided by email or commenting on the relevant issue and the latter will be made public and closed as `Won't fix`.

   b. **Acceptance**: If the team accepts the report, a CVE identifier will be requested through GitHub and a [private fork](https://docs.github.com/en/code-security/security-advisories/working-with-repository-security-advisories/collaborating-in-a-temporary-private-fork-to-resolve-a-repository-security-vulnerability) opened to work on a fix to the issue

3. **Resolution**: The team works to resolve the vulnerability in a
   timely manner. The timeline for resolution will depend on the
   complexity and severity of the vulnerability, but we will strive to
   address critical vulnerabilities as quickly as possible.

4. **Collaboration**: While working on a fix, the team maintains open and transparent
   communication with the reporter throughout the process, providing
   updates on the status of the vulnerability and any steps taken to
   remediate it. In particular this means that the reporter will be asked to review any proposed fix and to advise on the timing for public disclosure.

5. **Fixing Issue**: The team agrees on the fix, the announcement, and the release schedule with the reporter. If the reporter is not responsive in a reasonable time frame this should not block the team from moving to the next steps particularly in the face of a high impact or high severity issue.

   a. **Mitigation**: Depending on the severity and criticity of the issue, the team can decide to disclose the issue publicly in the absence of a fix _if and only if_ a clear, simple, and effective mitigation plan is defined. This _must_ include instructions for users and operators of the software, and a time horizon at which the issue will be properly fixed (eg. version number).

   b. **Fix**: When a fix is available and approved, it should be merged and made available as quickly as possible:

   - All commits to the private repository are squashed into a single commit whose description _should not_ make any reference it relates to a security vulnerability
   - A new Pull Request is created with this single commit
   - This PR's review and merging is expedited as all the work as already been done

6. **Release**: The team creates and publish a release that includes the fix

7. **Announcement**: Concomitant to the release announcement, the team announces the security vulnerability by making the GitHub issue public. This is the first point that any information regarding the vulnerability is made public.

   a. **Credit**: The team publicly acknowledges the contributions of the
   reporter once the vulnerability is resolved, subject to the
   reporter's preferences for attribution.

8. **Disagreements**: In case of disagreements with the reporter on the fix, mitigation, timing, or announcement, the team has the final say.

## Responsible Disclosure

We kindly request that reporters adhere to responsible disclosure
practices, which include:

- **Do not disclose the vulnerability publicly**: Please refrain from
  posting details of the vulnerability on public forums or social
  media until it has been resolved.
- **Do not exploit the vulnerability**: Do not attempt to exploit the
  vulnerability to cause harm or gain unauthorized access to systems.
- **Work with us**: Allow us a reasonable amount of time to
  investigate and address the vulnerability before publicly disclosing
  any details.

## Legal Protections

We will not pursue legal action against individuals who
report security vulnerabilities to us.

## Contact Information

To report a security vulnerability, please use [GitHub
form]((add project github form for your project)). Should you experience any issues reporting via GitHub or have other questions, Please contact [Security](security@intersectmbo.org).

## Revision of Policy

This Security Vulnerability Disclosure Policy may be updated or
revised as necessary. Please check the latest version of this policy
on the [xxxx repository]((add link for your project)).

## Conclusion

The xxxx project greatly appreciates the assistance of the security
community in helping us maintain the security of our software while
upholding the highest standards of privacy. Together, we can work to
identify and address vulnerabilities, ensuring a safer and more secure
experience for all users.

