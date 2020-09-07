with Lower_Layer_UDP;

package Server_Handler is

   package LLU renames Lower_Layer_UDP;

   procedure Handler (From     : in LLU.End_Point_Type;
                      To       : in LLU.End_Point_Type;
                      P_Buffer : access LLU.Buffer_Type);
   
   procedure Show_Active;
   
   procedure Show_Old;

end Server_Handler;
