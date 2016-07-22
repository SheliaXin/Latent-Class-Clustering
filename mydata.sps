DATA LIST FILE= "mydata.txt"  free (",")
/ V_Clustr V1_Cont V2_Cont V3_Cont V4_Ord V5_Ord V6_Nom V7_Nom V8_Nom V9_Count V10_Cont V11_Cont  .

VARIABLE LABELS
V_Clustr "V_Cluster" 
 V1_Cont "V1_Cont" 
 V2_Cont "V2_Cont" 
 V3_Cont "V3_Cont" 
 V4_Ord "V4_Ord" 
 V5_Ord "V5_Ord" 
 V6_Nom "V6_Nom" 
 V7_Nom "V7_Nom" 
 V8_Nom "V8_Nom" 
 V9_Count "V9_Count" 
 V10_Cont "V10_Count" 
 V11_Cont "V11_Count" 
 .

EXECUTE.
