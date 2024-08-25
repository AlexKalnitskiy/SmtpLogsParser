#r "nuget: FParsec"
#load "Parsers/DomainParser.fs"
#load "Parsers/EmailParser.fs"
#load "Parsers/IPParser.fs"
#load "Parsers/SmtpCodeParser.fs"
#load "Token.fs"
#load "MetaString.fs"
#load "Parsers/UrlParser.fs"
#load "Parsers/DateTimeParser.fs"
#load "Parsers/SmtpResponseParser.fs"

open FParsec
open SmtpLogsParser
open SmtpLogsParser.Parsers.SmtpResponseParser

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn $"Success: {MetaString.render result}"
    | Failure(errorMsg, _, _) -> printfn $"Failure: {errorMsg}"

let logLines =
    [
      "550- Mailbox unavailable or access denied [MJ8vvB3Z9HILDyk+wodtP4OmNhYCOy7YpEZnRextO1oq5vW0RCUUNrDzqw1hdKV1Lg== IP: 185.195.169.206]. https://service.mail.qq.com/detail/0/166."
      "554-5.7.1 <s1.mbx-sender-02.ru[185.99.9.172]>: Client host rejected: #652 SPAM mindbox.ru. For assistance, call +7(495)788-0999 int.755911. Please provide the following information in your problem report: time (Aug 22 14:05:10, MSK), client (185.99.9.172) and server (relay6.alfastrah.ru)."
      "550- Rejected: signature_incorrect"
      "550- rejected because 193.243.174.181 is in a black list at zen.spamhaus.org\nListed by CSS, see https://check.spamhaus.org/query/ip/193.243.174.181"
      "550-5.7.0 SPAM type=spam - Not spam? Contact spamadmin@mediabeam.net with reference ID 149169::1724324653-C97F911D-9B004426/10/56527156675 to resolve this issue."
      "550- rejected because 185.99.9.136 is in a black list at zen.spamhaus.org\nListed by CSS, see https://check.spamhaus.org/query/ip/185.99.9.136"
      "550- \"No such user / У нас нет такого почтового ящика\""
      "550- Mailbox unavailable or access denied [MJ8vvB3Z9HILDyk+wodtP4NN5hlvVmA/HTz1q10yWXm7nKV2VI8TUoIBD2qzvweSUg== IP: 193.243.174.182]. https://service.mail.qq.com/detail/0/166."
      "550- \"Sorry, no messages accepted due to spam attack\""
      "540-5.7.1<mensdelelecces@rambler.ru..>: recipient address rejected: Inactive"
      "540-5.7.1 <mensd2elelecces@rambler.ru>: recipient address rejected: Inactive"
      "540-5.7.1 <mensdele2lecces@rambler.ru>: recipient address rejected: Inactive"
      "540-5.7.1 <mensdelelecces@rambler.ru>: recipient address rejected: Inactive"
      "540-5.7.1 <mensde2lelecces@rambler.ru>: recipient address rejected: Inactive"
      "540-5.7.1 <mens5delelecces@rambler.ru>: recipient address rejected: Inactive"
      "451- 4.7.652 The mail server [88.216.57.52] has exceeded the maximum number of connections.    (S3115) [DU6PEPF0000A7E0.eurprd02.prod.outlook.com 2024-08-01T10:08:12.503Z 08DCAFACFDA32292]"
      "451- 4.7.652 The mail server [88.216.57.47] has exceeded the maximum number of connections. (S3115) [AM4PEPF00025F95.EURPRD83.prod.outlook.com 2024-08-01T10:07:56.868Z 08DCAF99A3203CA8]"
      "451- 4.7.652 The mail server [88.216.57.27] has exceeded the maximum number of connections. (S3355) [AM4PEPF00025F95.EUR0D83.prod.outlook.com 2024-08-01T10:39:56.868Z 08DCAF99A3203CA8]"
      "451- 4.7.652 The mail server [88.216.57.27] has exceeded the maximum number of connections. (44g6) [AM4PEPF00025F95.23D83.prod.outlook.com 2024-08-01T10:29:56.868Z 08DCAF99A3203CA8]"
      "451-4.3.2 Temporary server error. Please try again later ATTR17 [DU6PEPF0000B61C.eurprd02.prod.outlook.com 2024-08-01T09:32:01.894Z 08DCAFB9AE5EA650]"
      "451-4.3.2 Temporary server error. Please try again later ATTR17 [00B61CB9AE5EA650.eurprd02.prod.outlook.com 2024-08-01T09:32:01.894Z AE5EA650000B61C]"
      "451-4.4.3 Temporary server error. Please try again later ATTR2 [BN2PEPF00004FBF.namprd04.prod.outlook.com 2024-07-31T21:10:49.094Z 08DCAFC4626DA37A]"
      "451-4.4.3 Temporary server error. Please try again later ATTR2 [BN2PEP08DCAFC46FBF.namprd04.prod.outlook.com 2024-07-31T21:10:49.094Z 08DCA2PEPF0A37A]"
      "451-4.7.0 Temporary server error. Please try again later. PRX9 RemoteHost: PH7P222CA0012. ErrorCode 10054 [SA208DCAFDD015C7.namprd03.prod.outlook.com 2024-07-31T13:33:32.734Z 08DCAFDD636EC8D7]"
      "451-4.7.0 Temporary server error. Please try again later. PRX9 RemoteHost: PH7P222CA0012. ErrorCode 10054 [SA2PEPF000015C7.namprd03.prod.outlook.com 2024-07-31T13:33:32.734Z 08PEPF00636PEPF00D7]"
      "451-4.7.0 Temporary server error. Please try again later. D7RX9 RemoteHost: PH7P222CA0012. ErrorCode 10054 [SA2PEPF000015C7.namprd03.prod.outlook.com 2024-07-31T13:33:32.734Z 08PEPF00636PEPF00D7]"
      "451-4.3.0 Try again later 1722509182-GkULpI6Ax8c0-KgMovYjw"
      "451-4.3.0 Try again later 1725688609-GkUtyLpI-87Ax8c0-KgYjw"
      "421- Try again later (193.58.159.185). Please contact abuse@corp.mail.ru ."
      "421- Try again later (193.54.179.175). Please contact abuse@corp.mail.ru."
      "550- Message was not accepted -- invalid mailbox. Local mailbox ev.kosykh-2005@mail.ru is unavailable: user not found"
      "550-5.7.1 [185.99.9.147 12] Gmail has detected that this message is likely unsolicited mail. To reduce the amount of spam sent to Gmail, this message has been blocked. For more information, go to https://support.google.com/mail/?p=UnsolicitedMessageError 38308e7fff4ca-2f03d196db7si15964611fa.640 - gsmtp"
      "451- 4.7.26 Unauthenticated email from email.kazanexpress.ru is not accepted due 4.7.26 to domain's DMARC policy, but temporary DNS failures prevent 4.7.26 authentication. Please contact the administrator of email.kazanexpre 4.7.26 ss.ru domain if this was a legitimate mail. To learn about the DMARC 4.7.26 initiative, go to 4.7.26 https://support.google.com/mail/?p=DmarcRejection 38308e7fff4ca-2f03d187980si3400341fa.593 - gsmtp" ]

logLines |> Seq.iter (test smtpResponseParser)
