{application, smtp_client,     
        [{description, "a simple SMTP client implemented completely in Erlang."},
         {vsn, "1.1"},
         {modules, [    smtp_fsm,
                        email_msg
                        ]},

         {registered, []},
         {applications, [kernel,stdlib,crypto,inets]}
]}.

