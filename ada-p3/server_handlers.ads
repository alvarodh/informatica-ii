with Lower_Layer_UDP;

package Server_Handlers is

    package LLU renames Lower_Layer_UDP;

    procedure Handler (From     : in  LLU.End_Point_Type;
                       To       : in  LLU.End_Point_Type;
                       P_Buffer : access LLU.Buffer_Type);

    procedure Print_Active_Clients;

    procedure Print_Old_Clients;

end Server_Handlers;
