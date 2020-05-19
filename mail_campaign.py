import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import codecs


def customize(text, row):
    customized_mail_text = text.replace('[contact("first name")]', row['First Name'])
    customized_mail_text = customized_mail_text.replace('[invite("custom 1")]',
                                                        row['Invite Custom Field 1'])
    return customized_mail_text.replace('[invite(survey_link)]', row['Invite Link'])


def send_mail(row):
    with smtplib.SMTP('dutmail.tudelft.nl', 25) as server:
        msg = MIMEMultipart()
        from_addr = 'Frenk van Mil <f.c.j.vanmil@student.tudelft.nl>'
        to_addr = row['Email']
        msg['From'] = from_addr
        msg['Subject'] = 'Can we infer developer personality from software development activities?'
        mail_file = codecs.open('data/email.txt', 'r')
        mail_text = mail_file.read()
        msg['To'] = to_addr
        customized_mail_text = customize(mail_text, row)
        msg.attach(MIMEText(customized_mail_text, 'plain'))

        try:
            server.sendmail(from_addr, to_addr, msg.as_string())
            print('Mail Sent ' + to_addr)
        except Exception:
            print('Mail Not Sent ' + to_addr)

# import smtplib
# from email.mime.multipart import MIMEMultipart
# from email.mime.text import MIMEText
# import csv
# import codecs
#
# email=[]
# name=[]
# projectA=[]
# url=[]
#
# with open('survey.csv','r') as csvfile:
#     listreader=csv.reader(csvfile)
#     for row in listreader:
#         email.append(row[0])
#         name.append(row[1])
#         projectA.append(row[2])
#         url.append(row[3])
#
# with smtplib.SMTP('dutmail.tudelft.nl', 25) as server:
#     for i in range(len(email)):
#         fromaddr = 'Ayushi Rastogi <a.rastogi@tudelft.nl>'
#         msg = MIMEMultipart()
#         msg['From'] = fromaddr
#         msg['Subject'] = 'Subject goes here'
#         mail_file=codecs.open('developers.txt','r')
#         mail_text=mail_file.read()
#         toaddr=email[i]
#         msg['To'] = toaddr
#         customised_mail_text=mail_text
#         customised_mail_text = customised_mail_text.replace('<name>',name[i])
#         customised_mail_text = customised_mail_text.replace('<projectA>',projectA[i])
#         customised_mail_text = customised_mail_text.replace('<url>',url[i])
#         msg.attach(MIMEText(customised_mail_text,'plain'))
#
#         try:
#             server.sendmail(fromaddr, toaddr, msg.as_string())
#             print('Mail Sent '+ toaddr)
#         except Exception:
#             print('Mail Not Sent ' + toaddr)
