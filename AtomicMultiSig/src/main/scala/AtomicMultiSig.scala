import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import sigmastate.eval.CostingSigmaDslBuilder.GroupElement
import sigmastate.interpreter.CryptoConstants._
import java.math.BigInteger

object AtomicMultiSig {
  private def atomicDeposit(ConfigFileName: String): String = {
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
      val srGX = new BigInteger(config.getParameters.get("srGX"))
      val srGY = new BigInteger(config.getParameters.get("srGY"))
      val srG = GroupElement(dlogGroup.curve.createPoint(srGX, srGY))
      val ssGX = new BigInteger(config.getParameters.get("ssGX"))
      val ssGY = new BigInteger(config.getParameters.get("ssGY"))
      val ssG = GroupElement(dlogGroup.curve.createPoint(ssGX, ssGY))
      val krGX = new BigInteger(config.getParameters.get("krGX"))
      val krGY = new BigInteger(config.getParameters.get("krGY"))
      val krG = GroupElement(dlogGroup.curve.createPoint(krGX, krGY))
      val ksGX = new BigInteger(config.getParameters.get("ksGX"))
      val ksGY = new BigInteger(config.getParameters.get("ksGY"))
      val ksG = GroupElement(dlogGroup.curve.createPoint(ksGX, ksGY))
      val receiver = Address.create(config.getParameters.get("p2Addr")).getPublicKey
      val lockHeight = ctx.getHeight + 10
      val atomicLockScript: String = {
      s"""
            {
              val srBYTES = OUTPUTS(0).R4[Coll[Byte]].get
              val sr = byteArrayToBigInt(srBYTES)
              val ssBYTES = OUTPUTS(0).R5[Coll[Byte]].get
              val ss = byteArrayToBigInt(ssBYTES)
              val receiver_krG = OUTPUTS(0).R6[GroupElement].get
              val receiver_ksG = OUTPUTS(0).R7[GroupElement].get
              val G = decodePoint(generator)
              sigmaProp(
                receiver &&
                G.exp(sr) == srG &&
                G.exp(ss) == ssG &&
                receiver_krG == krG &&
                receiver_ksG == ksG ||
                sender && (HEIGHT > lockHeight)
              )
            }
        """.stripMargin
    }
    val contract: ErgoContract = ctx.compileContract(
      ConstantsBuilder.create()
        .item("sender", sender.getPublicKey)
        .item("receiver", receiver) //receiver is sender just for this example
        .item("srG", srG)
        .item("ssG", ssG)
        .item("krG", krG)
        .item("ksG", ksG)
        .item("generator", generator)
        .item("lockHeight", lockHeight)
        .build(),
      atomicLockScript)
      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()
      val AtomicBox = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outBoxBuilder()
        .value(ergoAmountFeeIncluded)
        .contract(contract)
        .build()
      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(AtomicBox)
        .sendChangeTo(sender.getErgoAddress)
        .build()
      ctx.sendTransaction(prover.sign(unsignedTransaction))

      prover.sign(unsignedTransaction).toJson(true)
    })
    txJson
    }
  private def atomicReceiverClaim(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("p2Addr"))
      val addrIndex: Int = config.getParameters.get("p2AddrIndex").toInt
      val walletMnemonic: String = config.getParameters.get("p2Mnemonic").toString
      val mnemonicPassword: String = config.getParameters.get("p2MnemonicPassword").toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val atomicBox: String = config.getParameters.get("atomicBox").toString
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      val krGX = new BigInteger(config.getParameters.get("krGX"))
      val krGY = new BigInteger(config.getParameters.get("krGY"))
      val krG = GroupElement(dlogGroup.curve.createPoint(krGX, krGY))
      val ksGX = new BigInteger(config.getParameters.get("ksGX"))
      val ksGY = new BigInteger(config.getParameters.get("ksGY"))
      val ksG = GroupElement(dlogGroup.curve.createPoint(ksGX, ksGY))
      val sr = config.getParameters.get("sr")
      val big_sr = new BigInteger(sr)
      val sr_ar = BigInt.javaBigInteger2bigInt(big_sr).toByteArray
      val ev_sr = ErgoValue.of(sr_ar)
      val ss = config.getParameters.get("ss")
      val big_ss = new BigInteger(ss)
      val ss_ar = BigInt.javaBigInteger2bigInt(big_ss).toByteArray
      val ev_ss = ErgoValue.of(ss_ar)
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(ev_sr, ev_ss, ErgoValue.of(krG), ErgoValue.of(ksG))
        .build()

      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(atomicBox).array.last)

      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      val signed = prover.sign(tx)

      ctx.sendTransaction(signed)

      signed.toJson(true)

    })
    txJson
  }
  private def atomicSenderReClaim(ConfigFileName: String): String = {
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
      val atomicBox: String = config.getParameters.get("atomicBox").toString

      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg * 2 - Parameters.MinFee
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted - Parameters.MinFee)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .registers(
          ErgoValue.of(BigInteger.ZERO.toByteArray),
          ErgoValue.of(BigInteger.ZERO.toByteArray),
          ErgoValue.of(dlogGroup.generator),
          ErgoValue.of(dlogGroup.generator))
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(atomicBox).array.last)
      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      val signed = prover.sign(tx)

      ctx.sendTransaction(signed)

      signed.toJson(true)
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
    val txJson = atomicDeposit(args(0))
    //val txJson = atomicReceiverClaim(args(0))
    //val txJson = atomicSenderReClaim(args(0))
    print(txJson)
  }
}
