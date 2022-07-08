# Cardano RTView

RTView is a part of `cardano-tracer` [service](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md). It is a real-time monitoring tool for Cardano nodes (RTView is an abbreviation for "Real Time View"). It provides an interactive web page where you can see different kinds of information about connected nodes.

# Contents

1. [Introduction](#Introduction)
   1. [Motivation](#Motivation)
   2. [Overview](#Overview)
2. [Configuration](#Configuration)
3. [Notifications](#Notifications)
   1. [SMTP settings](#SMTP-settings)
   2. [Note for Gmail users](#Note-for-Gmail-users)
   3. [Events](#Events)
   4. [Notify period](#Notify-period)
4. [UI](#UI)
   1. [Security Alert](#Security-Alert)

# Introduction

## Motivation

For a long time, Stake Pool Operators used third-party tools for monitoring their Cardano nodes, such as [Grafana](https://grafana.com/grafana/dashboards/12469)-based installations. These third-party solutions work, but they have two main problems:

1. Complex setup, especially for non-technical person.
2. Limited kinds of displayed information. For example, metrics can be shown, but error messages cannot.

RTView solves both of them:

1. Its setup is as simple as possible: if you have `cardano-tracer` installed, you already have RTView.
2. Because of using special network protocols integrated into the node, RTView can display any information that the node can provide.

## Overview

You can think of RTView as a SPA (Single Page Application) that can be opened in any modern browser. All the information on it changes dynamically, so you shouldn't refresh it.

When you open it for the first time, you'll see a help message about required configuration.

After your node connects to `cardano-tracer`, you'll see a column with different information, such as the node's name, version, protocol, era, sync percentage, KES values, blockchain info, etc. There is a separate column for each connected node, so you can see and compare their data.

Also, there are dynamic charts for different metrics, such as system metrics, blockchain metrics, transaction metrics, etc.

# Configuration

Since RTView is a part of `cardano-tracer`, the only thing you need to do is to enable RTView (because it's disabled by default). To do it, please add the following lines to `cardano-tracer`'s configuration file.

If you use `json`-configuration:

```
"hasRTView": {
  "epHost": "127.0.0.1",
  "epPort": 3300
}
```

Or, if you use `yaml`-configuration:

```
hasRTView:
  epHost: 127.0.0.1
  epPort: 3300
```

Here `epHost` and `epPort` specify the host and the port for RTView web page.

That's it. Now run `cardano-tracer` and open [127.0.0.1:3300](https://127.0.0.1:3300) in your browser.

# Notifications

RTView can send notifications about specified events (for example, warnings or errors). Click on the bell icon on the top bar to see the corresponding settings.

## SMTP settings

Technically, RTView contains an email client that sends emails using SMTP. That's why you need the SMTP settings of your email provider. Please fill in all the inputs marked with an asterisk in `Settings` window.

You can use `Send test email` button to check if your email settings are correct.

## Note for Gmail users

If you want to set up email notifications using your Gmail account, please make sure that `2-Step Verification` is enabled. You can check it in `Google Account` -> `Security`. After you enabled `2-Step Verification`, please generate the new app password (if you don't have one already) in `Security` -> `App passwords`. You'll need this app password for RTView settings.

Now you can set up RTView notifications:

1. `SMTP host`: `smtp.gmail.com`

2. `SMTP port`: `587`

3. `Username`: most likely, it's your email address

4. `Password`: app password you've generated

5. `SSL`: `STARTTLS`

## Events

When you click on the bell icon on the top bar, you can open `Events` window. Here you can specify events you want to be notified about.

For example, let's have a look at `Warnings` (i.e. all the messages from the node with `Warning` severity level). By default, the corresponding switch is disabled, which means that you won't be notified about warnings at all. But if you enable that switch, you will periodically receive a notification about warnings, if any.

You can use a switch `All events` in the bottom of the window to enable/disable all the events at once. Please note that if you disable all the events, the bell icon on the top bar becomes "slashed".

## Notify period

You can specify how frequently you want to receive notifications for a specific event. To do it, select a value from the dropdown list at the right of the event switch. There are values from `Immediately` to `Every 12 hours`.

If you selected `Immediately`, the new email with the associated event(s) will be sent right away. It can be used for critical events: most likely, you want to know about such events as soon as possible.

If you selected `Every 12 hours`, the new email with associated event(s) will be sent only two times a day. I can be used for non-critical events, like `Warnings`.

# UI

## Security Alert

When you open the web page for the first time, you'll see a warning from your browser, something like "Potential Security Risk Ahead" or "Your connection is not private". This is because `https`-connection between your browser and RTView is using [self-signed certificate](https://en.wikipedia.org/wiki/Self-signed_certificate) generated by [openssl](https://www.openssl.org/) program. So click to `Advanced` button and open the page. Technically, there is no risk at all - your connection **is** private.
