// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
// Try also http://fsharpforfunandprofit.com 
#nowarn "9"
open glfw3
open SharpVk
open System

open System.Runtime.InteropServices;
open System.Security;

[<AutoOpen>]
module Utils = 
    let nullptr = IntPtr.Zero
    let nullptru = UIntPtr.Zero

module Enum =
    let HasFlag flag (_enum: 'a when 'a :> Enum) = _enum.HasFlag flag
    let HasFlags flags (_enum: 'a when 'a :> Enum) = 
        flags |> Array.exists _enum.HasFlag

module Array = 
    let notEmpty<'a> = Array.isEmpty<'a> >> not

type glfw3.GLFWwindow with
    member self.GetSizeU () =
        let w, h = self.GetSize()
        uint32 w, uint32 h
    
    member self.Close () =
        glfw3.Glfw.DestroyWindow self

type Bool32 with
    static member True = Bool32 true
    static member False = Bool32 false

type Instance =
    inherit SharpVk.Instance

    interface IDisposable with
        member self.Dispose() =
            printfn "destroying instance"
            self.Destroy()

type SwapchainDetails =
    {   Format : Format
        Extent : Extent2D }

[<NoComparison>]
type VulkanApplication = 
    {   Instance : SharpVk.Instance
        PhysicalDevice : PhysicalDevice
        LogicalDevice : Device
        GraphicsQueue : Queue 
        PresentationQueue : Queue 
        TransferQueue : Queue 
        Swapchain : Swapchain 
        SwapchainImages : Image [] 
        SwapchainDetails : SwapchainDetails
        ImageViews : ImageView []
        RenderPass : RenderPass 
        PipelineLayout : PipelineLayout
        GraphicsPipeline : Pipeline
        VertexShader : ShaderModule
        FragmentShader : ShaderModule
        Framebuffers : Framebuffer [] 
        CommandPool : CommandPool 
        CommandBuffers : CommandBuffer [] 
        ImageAvailableSemaphore : Semaphore
        RenderFinishedSemaphore : Semaphore }

module VulkanApplication =
    let cleanup (application:VulkanApplication) = 
        // Wait for idle
        application.LogicalDevice.WaitIdle()
        application.CommandBuffers 
        |> ArrayProxy<CommandBuffer>.op_Implicit 
        |> application.CommandPool.FreeCommandBuffers
        application.ImageAvailableSemaphore.Destroy()
        application.RenderFinishedSemaphore.Destroy()
        application.VertexShader.Destroy()
        application.FragmentShader.Destroy()
        application.GraphicsPipeline.Destroy()
        application.PipelineLayout.Destroy()
        application.RenderPass.Destroy()
        application.Framebuffers |> Array.iter (fun f -> f.Destroy())
        application.CommandPool.Destroy()
        application.ImageViews |> Array.iter (fun i -> i.Destroy())
        application.Swapchain.Destroy()
        application.LogicalDevice.Destroy()
        application.Instance.Destroy()
        ()

[<NoComparison>]
type SwapChainSupportDetails =
    {   Capabilities : SurfaceCapabilities
        Formats : SurfaceFormat []
        PresentModes : PresentMode [] }

type QueueFamilyIndices =
    {   GraphicsFamily : uint32
        PresentFamily : uint32 }

module QueueFamilyIndices =
    let complete (a:QueueFamilyIndices) =
         a.GraphicsFamily >= 0u &&
         a.PresentFamily >= 0u
        
[<SuppressUnmanagedCodeSecurity>]
[<DllImport("glfw3", CallingConvention = CallingConvention.Cdecl,
 EntryPoint = "glfwCreateWindowSurface")>]
extern VkResult CreateWindowSurface(unativeint instance, unativeint window, unativeint allocator, uint64& surface)

let createWindowSurface(instance, window, allocator) = 
    let mutable surface = 0ul |> uint64
    CreateWindowSurface(instance, window, allocator, &surface), surface


// Unfortunately GLFW3.NET provides only IntPtrs and not UIntPtrs. Therefore some conversions have to be made.
let createSurface (instance:SharpVk.Instance) (window:glfw3.Window) =
    // A reinterpret cast is needed to acquire the IntPtr from the structure
    let instancePtr = instance.RawHandle.ToUInt64() |> unativeint

    let __, surface = createWindowSurface(instancePtr, window.__Instance |> unativeint, nullptru)

    SharpVk.Surface.CreateFromHandle(instance, surface)

let initVulkan (windowHandle:glfw3.Window) =
    // global values
    let validationLayers = 
        [|  //"VK_LAYER_LUNARG_api_dump"
            "VK_LAYER_LUNARG_standard_validation"
        |]
    let deviceExtensions = [| "VK_KHR_swapchain" |]

    let WIDTH, HEIGHT = windowHandle.GetSizeU ()

    let findQueueFamilies (physicalDevice:SharpVk.PhysicalDevice) surface =
        let properties = 
            physicalDevice.GetQueueFamilyProperties()
        let graphics =
            properties
            |> Array.tryFindIndex (fun prop -> prop.QueueCount > 0u && prop.QueueFlags.HasFlag SharpVk.QueueFlags.Graphics)
            |> Option.defaultValue -1 |> uint32
        let presentation =
            let rec find index =
                if properties.Length > int index then
                    let prop = properties.[int index]
                    if prop.QueueCount > 0u && physicalDevice.GetSurfaceSupport(index, surface) |> Bool32.op_Implicit then
                        index
                    else
                        find index + 1u
                else
                    UInt32.MaxValue // produce an invalid value, -1 isn't valid due to F#'s strict type checking
            find 0u
        {   GraphicsFamily = graphics
            PresentFamily = presentation }
    
    let checkDeviceExtensionSupport (device:SharpVk.PhysicalDevice) =
        let extensionProperties = device.EnumerateDeviceExtensionProperties null
        let fastExtensionProperties = extensionProperties |> Array.map (fun a -> a.ExtensionName) |> Collections.Generic.HashSet
        let rec CheckExtensionSupport extensionList =
            match extensionList |> Array.tryHead with
            | Some element ->
                if fastExtensionProperties.Contains element then
                    CheckExtensionSupport (extensionList |> Array.skip 1)
                else
                    false
            | None -> true
        CheckExtensionSupport deviceExtensions

    let querySwapChainSupport (device:PhysicalDevice) surface =
        {   Capabilities = device.GetSurfaceCapabilities surface
            Formats = device.GetSurfaceFormats surface
            PresentModes = device.GetSurfacePresentModes surface }

    let isSwapchainAdequate device surface =
        let details = querySwapChainSupport device surface
        details.Formats |> Array.notEmpty && details.PresentModes |> Array.notEmpty

    let isDeviceSuitable (device:SharpVk.PhysicalDevice) surface =
        let features = device.GetFeatures()
        let properties = device.GetProperties()
        let s = features.GeometryShader
        let r = s |> Bool32.op_Implicit

        QueueFamilyIndices.complete (findQueueFamilies device surface) &&
        checkDeviceExtensionSupport device && 
        isSwapchainAdequate device surface

    let chooseSwapSurfaceFormat availableFormats =
        if availableFormats |> Array.isEmpty then failwith "availableFormats array is empty"
        if availableFormats |> Array.length<SurfaceFormat> = 1 && availableFormats.[0].Format = Format.Undefined then
            SurfaceFormat (Format.B8G8R8A8UNorm, ColorSpace.SrgbNonlinear)
        else
            availableFormats 
            |> Array.tryFind 
                (fun f -> f.Format = Format.B8G8R8A8UNorm && f.ColorSpace = ColorSpace.SrgbNonlinear)
            |> Option.defaultValue (availableFormats |> Array.head)
    
    let chooseSwapPresentMode availablePresentModes =
        let pref = 
            [|  PresentMode.Mailbox
                PresentMode.Immediate |]
        availablePresentModes
        |> Array.tryFind<PresentMode> (Enum.HasFlags pref)
        |> Option.defaultValue PresentMode.Fifo

    let chooseSwapExtent (capabilities:SurfaceCapabilities) =
        if capabilities.CurrentExtent.Width <> UInt32.MaxValue then 
            capabilities.CurrentExtent
        else
            let minImageExtent = capabilities.MinImageExtent
            let maxImageExtent = capabilities.MaxImageExtent
            let w = min maxImageExtent.Width WIDTH |> max minImageExtent.Width
            let h = min maxImageExtent.Height HEIGHT |> max minImageExtent.Height
            Extent2D (w, h)
    
    let createLogicalDeviceQueuesAndSwapchain physicalDevice surface =
        let queueFamilyIndices = findQueueFamilies physicalDevice surface
        let sameQueues = queueFamilyIndices.GraphicsFamily = queueFamilyIndices.PresentFamily
        let indices = 
            if sameQueues then 
                [|  queueFamilyIndices.GraphicsFamily |]
            else
                [|  queueFamilyIndices.GraphicsFamily
                    queueFamilyIndices.PresentFamily |]
        let queueCreateInfos = 
            indices  |> Array.map (fun i -> 
                DeviceQueueCreateInfo(
                    QueueFamilyIndex = i,
                    QueuePriorities = [| 1.f |]
                    ))
        let deviceCreateInfo =
            DeviceCreateInfo(
                QueueCreateInfos = queueCreateInfos,
                EnabledLayerNames = validationLayers,
                EnabledExtensionNames = deviceExtensions
                )
        // Create the logical device
        let device = physicalDevice.CreateDevice deviceCreateInfo
        // Now retrieve the queue's we specified before
        let queues = 
            if sameQueues then
                indices 
                |> Array.head 
                |> Array.create 2
                |> Array.map (fun i -> device.GetQueue (i, 0u))
            else
                indices |> Array.map (fun it -> device.GetQueue (it, 0u))
        (device, queues, queueFamilyIndices)

    let createInstance () =
        let version = SharpVk.Version(1, 0, 0)

        //Create an appinfo
        let appInfo = 
            ApplicationInfo(
                ApiVersion = version,
                ApplicationName = "SharpVk & GLFW test",
                EngineName = "SharpVk & GLFW test",
                EngineVersion = version
            )

        // Get the required extensions for surface creation
        let extensions = Glfw.GetRequiredInstanceExtensions(); 
        printfn "extensions: %A, count: %A" extensions (System.Convert.ToUInt32(extensions.Length))

        //Create an instance
        let instanceCreateInfo = 
            InstanceCreateInfo(
                ApplicationInfo = Nullable appInfo,
                EnabledExtensionNames = extensions,
                EnabledLayerNames = validationLayers
            )
        Instance.Create(instanceCreateInfo)
    
    let createSurface instance =
        createSurface instance windowHandle

    let pickPhysicalDevice (instance:SharpVk.Instance) surface =
        let devices = 
            instance.EnumeratePhysicalDevices()
        devices |> Array.tryFind (fun d -> isDeviceSuitable d surface)
        
        
    let createSwapchain physicalDevice surface (device:Device) =
        let swapChainSupport = querySwapChainSupport physicalDevice surface
        let capabilities = swapChainSupport.Capabilities

        let surfaceFormat = chooseSwapSurfaceFormat swapChainSupport.Formats
        let presentMode = chooseSwapPresentMode swapChainSupport.PresentModes
        let extent = chooseSwapExtent capabilities

        let imageCount = 
            let imageCount = capabilities.MinImageCount + 1u
            if capabilities.MaxImageCount > 0u && imageCount > capabilities.MaxImageCount then
                capabilities.MaxImageCount
            else imageCount
        
        let indices = findQueueFamilies physicalDevice surface
        let imageSharingMode,
            queueFamilyIndices =
                if indices.GraphicsFamily <> indices.PresentFamily then
                    SharingMode.Concurrent, [| indices.GraphicsFamily; indices.PresentFamily |]
                else
                    SharingMode.Exclusive, [| |]

        let createInfo = 
            SwapchainCreateInfo(
                Surface = surface,
                MinImageCount = imageCount,
                ImageFormat = surfaceFormat.Format,
                ImageColorSpace = surfaceFormat.ColorSpace,
                ImageExtent = extent,
                ImageArrayLayers = 1u,
                ImageUsage = ImageUsageFlags.ColorAttachment,
                ImageSharingMode = imageSharingMode,
                QueueFamilyIndices = queueFamilyIndices,
                PreTransform = capabilities.CurrentTransform,
                CompositeAlpha = CompositeAlphaFlags.Opaque,
                PresentMode = presentMode,
                Clipped = Bool32.True,
                OldSwapchain = null
            )

        device.CreateSwapchain createInfo, 
        {   Format = surfaceFormat.Format
            Extent = extent }
    
    let createImageViews (device:Device) (swapchainImages:Image []) (swapchainImageFormat:Format)= 
        [|  for image in swapchainImages do
                let createInfo = 
                    ImageViewCreateInfo(
                        Image = image,
                        ViewType = ImageViewType.ImageView2d,
                        Format = swapchainImageFormat,
                        Components = 
                            ComponentMapping ( ComponentSwizzle.Identity,ComponentSwizzle.Identity, ComponentSwizzle.Identity, ComponentSwizzle.Identity),
                        SubresourceRange = 
                            ImageSubresourceRange(ImageAspectFlags.Color, 0u, 1u, 0u, 1u)
                    )
                yield device.CreateImageView createInfo
            |]

    let createRenderPass (device:Device) swapchainImageFormat =
        let colorAttachment =
            AttachmentDescription(
                Format = swapchainImageFormat,
                Samples = SampleCountFlags.SampleCount1,
                LoadOp = AttachmentLoadOp.Clear,
                StoreOp = AttachmentStoreOp.Store,
                StencilLoadOp = AttachmentLoadOp.DontCare,
                StencilStoreOp = AttachmentStoreOp.DontCare,
                InitialLayout = ImageLayout.Undefined,
                FinalLayout = ImageLayout.PresentSource
            )

        let subpass = 
            SubpassDescription(
                PipelineBindPoint = PipelineBindPoint.Graphics,
                ColorAttachments = [| AttachmentReference(0u, ImageLayout.ColorAttachmentOptimal) |],
                DepthStencilAttachment = AttachmentReference(Attachment = Constants.AttachmentUnused)
            )

        let dependencies =
            SubpassDependency(
                SourceSubpass = 0u,
                DestinationSubpass = Constants.SubpassExternal,
                SourceStageMask = PipelineStageFlags.ColorAttachmentOutput,
                SourceAccessMask = (AccessFlags.ColorAttachmentRead ||| AccessFlags.ColorAttachmentWrite),
                DestinationStageMask = PipelineStageFlags.BottomOfPipe,
                DestinationAccessMask = AccessFlags.MemoryRead
                )
        
        let renderPassInfo = 
            RenderPassCreateInfo(
                Attachments = [| colorAttachment |],
                Subpasses = [| subpass |],
                Dependencies = [| dependencies |]
                )
        
        try
            device.CreateRenderPass renderPassInfo
        with e -> failwithf "error %A" e
    
    let createGraphicsPipeline (device:Device) (swapchainExtent:Extent2D) renderPass =
        let readShaderData path =
            let data = IO.File.ReadAllBytes path
            let mutable res = Array.create (data.Length / 4) 0u
            System.Buffer.BlockCopy(data, 0, res, 0, data.Length)
            res
        let vertShaderCode = readShaderData "vert.spv"
        let fragShaderCode = readShaderData "frag.spv"
        let createShaderModule (code:uint32[]) =
            let shaderModuleCreateInfo =
                ShaderModuleCreateInfo(
                    Code = code,
                    CodeSize = Size.op_Implicit (code.Length * 4)
                )
            device.CreateShaderModule shaderModuleCreateInfo
        let vertShader = createShaderModule vertShaderCode
        let fragShader = createShaderModule fragShaderCode

        let vertShaderStageInfo = 
            PipelineShaderStageCreateInfo(
                Stage = ShaderStageFlags.Vertex,
                Module = vertShader,
                Name = "main"
            )
        let fragShaderStageInfo = 
            PipelineShaderStageCreateInfo(
                Stage = ShaderStageFlags.Fragment,
                Module= fragShader,
                Name = "main"
            )
        let shaderStages = 
            [|  vertShaderStageInfo
                fragShaderStageInfo |]
        let vertexInputStageInfo =
            PipelineVertexInputStateCreateInfo(
                VertexBindingDescriptions = Array.empty,
                VertexAttributeDescriptions = Array.empty
            )
        
        let inputAssemblyStageInfo = 
            PipelineInputAssemblyStateCreateInfo(
                Topology = PrimitiveTopology.TriangleList,
                PrimitiveRestartEnable = Bool32.False
            )
        
        let viewport =
            Viewport(0.f, 0.f, float32 swapchainExtent.Width, float32 swapchainExtent.Height, 0.f, 1.f)
        
        let scissor =
            Rect2D(Offset2D(), swapchainExtent)
        
        let viewportStateInfo =
            PipelineViewportStateCreateInfo(
                Viewports = [| viewport |],
                Scissors = [| scissor |]
            )
        
        let rasterizerInfo = 
            PipelineRasterizationStateCreateInfo(
                DepthClampEnable = Bool32.False,
                RasterizerDiscardEnable = Bool32.False,
                PolygonMode = PolygonMode.Fill,
                LineWidth = 1.f,
                CullMode = CullModeFlags.Back,
                FrontFace = FrontFace.Clockwise,
                DepthBiasEnable = Bool32.False
            )
        
        let multisamplingInfo =
            PipelineMultisampleStateCreateInfo(
                SampleShadingEnable = Bool32.False,
                RasterizationSamples = SampleCountFlags.SampleCount1,
                MinSampleShading = 1.f
            )
        
        let colorBlendAttachmentInfo =
            PipelineColorBlendAttachmentState(
                ColorWriteMask = 
                    (ColorComponentFlags.R ||| ColorComponentFlags.G ||| ColorComponentFlags.B ||| ColorComponentFlags.A),
                BlendEnable = Bool32.False,
                SourceColorBlendFactor = BlendFactor.One,
                DestinationColorBlendFactor = BlendFactor.Zero,
                ColorBlendOp = BlendOp.Add,
                SourceAlphaBlendFactor = BlendFactor.One,
                DestinationAlphaBlendFactor = BlendFactor.Zero,
                AlphaBlendOp = BlendOp.Add
            )
        
        let colorBlendingInfo =
            PipelineColorBlendStateCreateInfo(
                LogicOpEnable = Bool32.False,
                Attachments = [| colorBlendAttachmentInfo |],
                LogicOp = LogicOp.Copy,
                BlendConstants = [| 0.f; 0.f; 0.f; 0.f |]
            )
        
        let dynamicStateInfo =
            PipelineDynamicStateCreateInfo(
                DynamicStates = 
                    [|  DynamicState.LineWidth
                        DynamicState.Viewport |]
            )
        
        let pipelineLayoutInfo = 
            PipelineLayoutCreateInfo()

        let pipelineLayout = device.CreatePipelineLayout pipelineLayoutInfo

        let graphicsPipelineInfo =
            GraphicsPipelineCreateInfo(
                Stages = shaderStages,
                VertexInputState = Nullable(PipelineVertexInputStateCreateInfo()),
                InputAssemblyState = Nullable inputAssemblyStageInfo, 
                ViewportState = Nullable viewportStateInfo,
                RasterizationState = Nullable rasterizerInfo,
                MultisampleState = Nullable multisamplingInfo,
                ColorBlendState = Nullable colorBlendingInfo,
                Layout = pipelineLayout,
                RenderPass = renderPass,
                Subpass = 0u
            )

        let pipelineInfos = graphicsPipelineInfo |> ArrayProxy<GraphicsPipelineCreateInfo>.op_Implicit
        device.CreateGraphicsPipelines (null, pipelineInfos) |> Array.head, vertShader, fragShader, pipelineLayout
    
    let createFramebuffer (device:Device) (swapchainExtent:Extent2D) swapchainImageViews renderPass =
         [| for imgView in swapchainImageViews do
                let framebufferInfo =
                    FramebufferCreateInfo(
                        RenderPass = renderPass,
                        Attachments = [| imgView |],
                        Width = swapchainExtent.Width,
                        Height = swapchainExtent.Height,
                        Layers = 1u
                    )
                yield device.CreateFramebuffer framebufferInfo
         |]

    let createCommandPool (device:Device) (queueFamilyIndices:QueueFamilyIndices) =
        let commandPoolInfo =
            CommandPoolCreateInfo(
                QueueFamilyIndex = queueFamilyIndices.GraphicsFamily,
                Flags = CommandPoolCreateFlags.None
            )
        device.CreateCommandPool commandPoolInfo
    
    let createCommandBuffers (device:Device) commandPool swapchainFramebuffers renderPass graphicsPipeline swapchainExtent =
        let allocInfo =
            CommandBufferAllocateInfo(
                CommandPool = commandPool,
                Level = CommandBufferLevel.Primary,
                CommandBufferCount = (swapchainFramebuffers |> Array.length |> uint32)
            )
        let commandBuffers = device.AllocateCommandBuffers allocInfo

        for i in 0 .. (commandBuffers.Length - 1) do
            let cmdBuffer = commandBuffers.[i]
            let beingInfo =
                CommandBufferBeginInfo(
                    Flags = CommandBufferUsageFlags.SimultaneousUse
                )
            
            cmdBuffer.Begin beingInfo

            let renderPassBeginInfo =
                RenderPassBeginInfo(
                    RenderPass = renderPass,
                    Framebuffer = swapchainFramebuffers.[i],
                    RenderArea = Rect2D(Offset2D(), swapchainExtent),
                    ClearValues = [| (ClearColorValue(0.f, 0.f, 0.f, 1.f) |> ClearValue.op_Implicit) |]
                )
            
            cmdBuffer.BeginRenderPass (renderPassBeginInfo, SubpassContents.Inline)
            cmdBuffer.BindPipeline(PipelineBindPoint.Graphics, graphicsPipeline)
            cmdBuffer.Draw(3u, 1u, 0u, 0u)
            cmdBuffer.EndRenderPass()
            cmdBuffer.End()
        commandBuffers

    async {
        let instance = createInstance()
        
        let surface = createSurface instance

        let physicalDevice = 
            let res = pickPhysicalDevice instance surface
            match res with
            | Some res -> res
            | _ -> failwith "No suitable physical device found"
            
        let device, queues, queueFamilyIndices = 
            createLogicalDeviceQueuesAndSwapchain physicalDevice surface 
        
        let swapchain, swapchainDetails = createSwapchain physicalDevice surface device
        let swapchainImages = swapchain.GetImages()
        let swapchainImageViews = createImageViews device swapchainImages swapchainDetails.Format

        let renderPass = createRenderPass device swapchainDetails.Format

        let graphicsPipeline, vertShader, fragShader, pipelineLayout = 
            createGraphicsPipeline device swapchainDetails.Extent renderPass
        
        let framebuffers = createFramebuffer device swapchainDetails.Extent swapchainImageViews renderPass

        let commandPool = createCommandPool device queueFamilyIndices

        let commandBuffers = createCommandBuffers device commandPool framebuffers renderPass graphicsPipeline swapchainDetails.Extent

        let imageAvailableSemaphore, renderFinishedSemaphore =
            let createInfo = SemaphoreCreateInfo()
            device.CreateSemaphore createInfo, device.CreateSemaphore createInfo

        return 
            {   Instance = instance
                PhysicalDevice = physicalDevice
                LogicalDevice = device
                GraphicsQueue = queues.[0]
                PresentationQueue = queues.[1]
                TransferQueue = null
                Swapchain = swapchain
                SwapchainImages = swapchainImages 
                SwapchainDetails = swapchainDetails 
                ImageViews = swapchainImageViews
                RenderPass = renderPass
                PipelineLayout = pipelineLayout
                GraphicsPipeline = graphicsPipeline
                VertexShader = vertShader
                FragmentShader = fragShader
                Framebuffers = framebuffers 
                CommandPool = commandPool 
                CommandBuffers = commandBuffers 
                ImageAvailableSemaphore = imageAvailableSemaphore
                RenderFinishedSemaphore = renderFinishedSemaphore }
    }

let TestSharpVkWithGLFW3dotNet () =
    // Disable OpenGL
    do Glfw.WindowHint(int State.ClientApi, int State.NoApi)
    // Create the glfw window
    use window = new Window(200, 200, "SharpVk & GLFW test");
    //// Test the size callback
    do window.SizeChanged.Add((fun (args) -> printfn "title: %A, width: %A, height: %A" args.source.Title args.width args.height)) 
    //// Test the key callback
    do window.KeyChanged.Add(fun args -> printfn "key: %A, modifiers_ %A" (Glfw.GetKeyName((int)Key.Unknown, args.scancode)) (Glfw.GetKeyModifiers(args.mods));) 
    printfn "Created GLFW-window."

    //Display the window
    do window.Show()
    
    let application = initVulkan window |> Async.RunSynchronously

    while window.ShouldClose() |> not do
        Glfw.PollEvents()
        let framebufferIndex = application.Swapchain.AcquireNextImage(UInt64.MaxValue, application.ImageAvailableSemaphore, null)
        let submitInfo =
            SubmitInfo(
                WaitSemaphores = [| application.ImageAvailableSemaphore |],
                WaitDestinationStageMask = [| PipelineStageFlags.ColorAttachmentOutput |],
                CommandBuffers = ( application.CommandBuffers.[int framebufferIndex ] |> Array.create 1 ),
                SignalSemaphores = [| application.RenderFinishedSemaphore |]
            )
        application.GraphicsQueue.Submit(submitInfo |> ArrayProxy.op_Implicit, null)
            
        let presentInfo =
            PresentInfo(
                WaitSemaphores = [| application.RenderFinishedSemaphore |],
                Swapchains = [| application.Swapchain |],
                ImageIndices = [| framebufferIndex |],
                Results = null
            )
        application.PresentationQueue.Present presentInfo 
        |> printfn "Present result %A"
            
        application.PresentationQueue.WaitIdle ()

    VulkanApplication.cleanup application

[<EntryPoint>] 
let main argv = 
    printfn "%A" argv
    TestSharpVkWithGLFW3dotNet ()
    Console.ReadKey() |> ignore
    0 // return an integer exit code
