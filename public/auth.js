const register = async () => {
    try {
        const startRegisterOptions = createStartRegisterOptions()
        const publicKeyCredentialCreationOptions = await startRegister(startRegisterOptions)
        const credentials = await createCredentials(publicKeyCredentialCreationOptions)
        const finishRegisterOptions = createFinishRegisterOptions(credentials)
        console.log(finishRegisterOptions)
        await finishRegister(finishRegisterOptions)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const startRegister = startRegisterOptions =>
    request("/register/start", startRegisterOptions)

const finishRegister = finishRequestOptions =>
    request("/register/finish", finishRequestOptions)

const createCredentials = options =>
    navigator.credentials.create({
        publicKey: {
            ...options,
            user: {
                ...options.user,
                id: fromBase64(options.user.id),
            },
            challenge: fromBase64(options.challenge),
        }
    })

const createStartRegisterOptions = () => ({
    name: document.getElementById("name").value,
    displayName: document.getElementById("displayName").value,
})

const createFinishRegisterOptions = credentials => ({
    id: toBase64(credentials.rawId),
    authenticatorAttachment: credentials.authenticatorAttachment,
    response: {
        attestationObject: toBase64(credentials.response.attestationObject),
        clientDataJSON: toBase64(credentials.response.clientDataJSON),
    },
    type: credentials.type
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
