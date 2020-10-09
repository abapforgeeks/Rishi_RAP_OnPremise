@AbapCatalog.sqlViewName: 'ZRISHIPO_M'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Purchase Doc Approval'
@ObjectModel.semanticKey: ['PurchaseDocument']
@ObjectModel.representativeKey: 'PurchaseDocument'
define root view ZI_RISHI_VALDET_DEMO_M
  as select from ZI_Rishi_PurchaseDocTotalPrice
{
      //ZI_Rishi_PurchaseDocTotalPrice
  key PurchaseDocument,
      TotalPrice,
      case when TotalPrice >= 0 and  TotalPrice < 250 then 3
      when  TotalPrice >= 250 and  TotalPrice < 500 then 2
      when TotalPrice >=500 then 1
      else 0 end                                       as PoPriceCriticality,

      case when TotalPrice > 500 then 'Yes' else 'No' end as IsApprovalReqiored,
      
      Currency,
      PurchaseDesc,
      PurchaseStatus,
      PurchasePrio,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      /* Associations */
      //ZI_Rishi_PurchaseDocTotalPrice
      _Currency,
      _Priority,
      _PurchaseItems,
      _Status
}
