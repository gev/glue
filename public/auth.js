const register = async () => {
    try {
        const startRegisterOptions = makeStartRegisterOptions()
        const publicKeyCredentialCreationOptions = await startRegister(startRegisterOptions)
        console.log(publicKeyCredentialCreationOptions)
        const credentials = await makeCredentials(publicKeyCredentialCreationOptions)
        console.log(credentials)
        console.log(new TextDecoder().decode(credentials.response.attestationObject))
        const finishRegisterOptions = makeFinishRegisterOptions(credentials)
        console.log(finishRegisterOptions)
        const res = await finishRegister(finishRegisterOptions)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const startRegister = startRegisterOptions =>
    request("/register/start", startRegisterOptions)

const finishRegister = finishRequestOptions =>
    request("/register/finish", finishRequestOptions)

const makeCredentials = options =>
    navigator.credentials.create({
        publicKey: makePublicKeyCredentialCreationOptions(options)
    })

const makeStartRegisterOptions = () => ({
    name: document.getElementById("name").value,
    displayName: document.getElementById("displayName").value,
})

const makeFinishRegisterOptions = credentials => ({
    id: credentials.id,
    authenticatorAttachment: credentials.authenticatorAttachment,
    response: {
        attestationObject: toBase64(credentials.response.attestationObject),
        clientDataJSON: toBase64(credentials.response.clientDataJSON),
    },
    type: credentials.type
})

const makePublicKeyCredentialCreationOptions = options => ({
    ...options,
    user: {
        ...options.user,
        id: fromBase64(options.user.id),
    },
    challenge: fromBase64(options.challenge),
})


const authenticate = () => {
    console.log("authenticate")
}

const request = async (url, data) => {
    const response = await fetch(url, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(data),
    })
    if (response.ok) {
        return response.json()
    }
    throw new Error(await response.text())
}

const fromBase64 = base64 =>
    Uint8Array.from(atob(base64), c => c.charCodeAt(0))

const toBase64 = data =>
    btoa(String.fromCharCode(...new Uint8Array(data)))


const debug = data => {
    const el = document.getElementById("debug")
    el.innerHTML =
        typeof data === "string"
            ? data
            : JSON.stringify(data, null, 4)
}
