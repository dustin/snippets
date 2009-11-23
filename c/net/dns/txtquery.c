#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <dns.h>
#include <dns_util.h>

int main(int argc, char **argv)
{
    dns_handle_t dns = dns_open(NULL);
    uint16_t dnstype = 0;
    if (dns_type_number("TXT", &dnstype) != 0) {
        fprintf(stderr, "Don't know what TXT type means.\n");
        exit(1);
    }
    uint16_t dnsclass = 0;
    if (dns_class_number("IN", &dnsclass) != 0) {
        fprintf(stderr, "Don't know what IN class means.\n");
        exit(1);
    }
    fprintf(stderr, "Doing lookup\n");
    dns_reply_t *reply = dns_lookup(dns, argv[1], dnsclass, dnstype);
    if (reply == NULL) {
        fprintf(stderr, "Couldn't find %s\n", argv[1]);
        exit(1);
    }
    fprintf(stderr, "Printing reply: %p (status=%d)\n", reply, reply->status);
    for (int i = 0; reply->answer[i]; i++) {
        dns_resource_record_t *rr = reply->answer[i];
        fprintf(stderr, "answer rr %p\n", rr);
        fprintf(stdout, "  Type:  %s (%d)\n", dns_type_string(rr->dnstype),
                rr->dnstype);
        if (rr->dnstype == dnstype) {
            fprintf(stderr, "  Data:  ");
            for (int j = 0; j < rr->data.TXT->string_count; j++) {
                if (j > 0) {
                    fprintf(stderr, ", ");
                }
                fprintf(stderr, "\"%s\"", rr->data.TXT->strings[j]);
            }
            fprintf(stderr, "\n");
        }
    }
}
