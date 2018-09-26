with Lower_Layer_UDP;

package Client_Handlers is

    package LLU renames Lower_Layer_UDP;

    procedure Handler (From     : in  LLU.End_Point_Type;
                       To       : in  LLU.End_Point_Type;
                       P_Buffer : access LLU.Buffer_Type);

end Client_Handlers;
