import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import org.ergoplatform.ErgoBox.R4
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract

import java.math.BigInteger
//import sigmastate.basics.SecP256K1.curve
import sigmastate.eval.CostingSigmaDslBuilder.GroupElement
import sigmastate.interpreter.CryptoConstants._
object GroupElementLock {
  private def groupElementLock(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("senderAddr"))
      val addrIndex: Int = config.getParameters.get("addrIndex").toInt
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeIncluded: Long = ergoAmount + Parameters.MinFee
      val X = new BigInteger(config.getParameters.get("geX"))
      val Y = new BigInteger(config.getParameters.get("geY"))
      val GE = GroupElement(dlogGroup.curve.createPoint(X, Y))
      val groupElementLockScript: String = {
        s"""
            {
            val receiverGE = OUTPUTS(0).R4[GroupElement].get
              sigmaProp(
                receiver &&
                receiverGE == contractGE
              )
            }
        """.stripMargin
      }
      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("receiver", sender.getPublicKey) //receiver is sender just for this example
          .item("contractGE", GE)
          .build(),
        groupElementLockScript)
      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()
      val groupElementLockBox = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outBoxBuilder()
        .value(ergoAmountFeeIncluded)
        .contract(contract)
        .build()
      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(groupElementLockBox)
        .sendChangeTo(sender.getErgoAddress)
        .build()

      ctx.sendTransaction(prover.sign(unsignedTransaction))

      prover.sign(unsignedTransaction).toJson(true)
    })
    txJson
  }

  private def groupElementUnlock(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("senderAddr"))
      val addrIndex: Int = config.getParameters.get("addrIndex").toInt
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val geBox: String = config.getParameters.get("geBox").toString
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeSubtracted: Long = ergoAmount - Parameters.MinFee
      val X = new BigInteger(config.getParameters.get("geX"))
      val Y = new BigInteger(config.getParameters.get("geY"))
      val GE = GroupElement(dlogGroup.curve.createPoint(X, Y))



      //val xBYTES = x.toByteArray()
      val eGE = ErgoValue.of(GE)
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(eGE)
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(geBox).array.last)
      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      ctx.sendTransaction(prover.sign(tx))

      prover.sign(tx).toJson(true)
    })
    txJson
  }
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      print("too many args, enter one arg for config file name")
      sys.exit
    }
    else if (args.length == 0) {
      print("enter the config file name as the only argument")
      sys.exit
    }
    val txJson = groupElementLock(args(0))
    //val txJson = groupElementUnlock(args(0))
    print(txJson)
  }
}
