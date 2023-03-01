import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import sigmastate.interpreter.CryptoConstants._
import java.math.BigInteger

object ScalarLock {
  private def scalarLock(ConfigFileName: String): String = {
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
      val generator = dlogGroup.generator.getEncoded(true)
      val x = new BigInteger(config.getParameters.get("x"))
      val xG = dlogGroup.generator.multiply(x)
      val scalarLockScript: String = {
        s"""
            {
            val xBYTES = OUTPUTS(0).R4[Coll[Byte]].get
            val x = byteArrayToBigInt(xBYTES)
            val G = decodePoint(generator)
              sigmaProp(
                receiver &&
                G.exp(x) == xG
              )
            }
        """.stripMargin
      }
      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("receiver", sender.getPublicKey) //receiver is sender just for this example
          .item("xG", xG)
          .item("generator", generator)
          .build(),
        scalarLockScript)
      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()
      val scalarLockBox= ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outBoxBuilder()
        .value(ergoAmountFeeIncluded)
        .contract(contract)
        .build()
      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(scalarLockBox)
        .sendChangeTo(sender.getErgoAddress)
        .build()

      ctx.sendTransaction(prover.sign(unsignedTransaction))

      prover.sign(unsignedTransaction).toJson(true)
    })
    txJson
  }

  private def scalarUnlock(ConfigFileName: String): String = {
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
      val scalarLockBox: String = config.getParameters.get("scalarLockBox").toString
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeSubtracted: Long = ergoAmount - Parameters.MinFee
      val x = new BigInteger(config.getParameters.get("x"))
      val xBYTES = x.toByteArray()
      val exBYTES = ErgoValue.of(xBYTES)
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted )
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(exBYTES)
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(scalarLockBox).array.last)
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
      println("too many args, enter one arg for config file name")
      sys.exit
    }
    else if (args.length == 0) {
      print("enter the config file name as the only argument")
      sys.exit
    }
    val txJson: String = scalarLock(args(0))
    //val txJson: String = scalarUnlock(args(0))
    println(txJson)
  }
}
